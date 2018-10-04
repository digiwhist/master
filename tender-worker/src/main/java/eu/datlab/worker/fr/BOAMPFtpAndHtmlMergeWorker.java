package eu.datlab.worker.fr;

import static eu.datlab.worker.fr.BOAMPTenderUtils.HTML_SOURCE_DATA_METADATA_KEY;
import static eu.datlab.worker.fr.BOAMPTenderUtils.HTML_SOURCE_URL_METADATA_KEY;

import java.net.URL;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dao.jdbc.JdbcTransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * This worker merges FTP and HTML publications in raw data table. See https://red.datlab.cz/redmine/issues/3743.
 * It creates records, where "createdBy" and "modifiedBy" is name of this worker. Run this query to set desired names:
 *   update digiwhist_test.raw_data
 *   set createdBy = 'eu.datlab.worker.fr.raw.BOAMPTenderDownloader',
 *   modifiedby = 'eu.datlab.worker.fr.raw.BOAMPTenderDownloader'
 *   where modifiedby = 'eu.datlab.worker.fr.BOAMPFtpAndHtmlMergeWorker';
 *
 * @author Marek Mikes
 */
public final class BOAMPFtpAndHtmlMergeWorker extends BaseWorker {

    private static final String INCOMING_EXCHANGE_NAME = "mergeWorkerInit";

    private static final String OUTGOING_EXCHANGE_NAME = "mergeWorkerOutgoingExchangeName";

    private static final String VERSION = "1";

    private RawDataDAO<RawData> rawDAO;

    private static final Integer PAGE_SIZE = 5000;

    /**
     * Initialization of everything.
     */
    public BOAMPFtpAndHtmlMergeWorker() {
        super();

        // handle commits manually, disable autocmmit feature
        try {
            JdbcTransactionUtils.getInstance().getConnection().setAutoCommit(false);
        } catch (SQLException e) {
            logger.debug("Unable to disable autocommit {}.", e);
            throw new UnrecoverableException("Unable to disable autocommit", e);
        }

        rawDAO = DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }

    @Override
    protected String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected String getOutgoingExchangeName() {
        return OUTGOING_EXCHANGE_NAME;
    }

    @Override
    public void doWork(final Message message) {
        // get IDs of FTP records to be processed
        List<String> ftpRawIds = rawDAO.getIdsBySourceAndVersion("eu.datlab.worker.fr.raw.BOAMPFtpTenderDownloader",
                "3");

        logger.info("Processing {} FTP raw data", ftpRawIds.size());

        final String startId = message.getValue("startId");
        if (startId != null) {
            logger.info("Start ID is entered. The first ID will be the start ID, because the worker may had not " +
                    "saved all publications for the record (the record can contain many publications)");
            ftpRawIds = ftpRawIds.subList(ftpRawIds.indexOf(startId), ftpRawIds.size());
        }

        // counter of new records
        Integer counter = 0;
        for (final String ftpRawId : ftpRawIds) {
            // get FTP record, find corresponding web publication(s), merge them and save as a new record(s)

            // get FTP record
            final Raw ftpRecord = rawDAO.getById(ftpRawId);

            final String fileName = ftpRecord.getSourceFileName();
            if (BOAMPTenderUtils.isNewXmlFormat(fileName)) {
                final String publicationSourceId = BOAMPTenderUtils.getPublicationSourceIdFrom(fileName);
                mergeAndSave(ftpRecord.getSourceData(), ftpRecord.getSourceUrl(), fileName, publicationSourceId);
                if (++counter % PAGE_SIZE == 0) {
                    commit(counter, ftpRawId);
                }
            } else {
                final Document document = Jsoup.parse(ftpRecord.getSourceData(), "", Parser.xmlParser());
                Elements publicationElements = JsoupUtils.select("ANNONCE_REF", document);
                for (Element publicationElement : publicationElements) {
                    // copy the element and try to merge

                    // without cloning, appending to a new element would remove the elements
                    // from original parent
                    final Element publicationElementCopy = publicationElement.clone();
                    final Element rootElement = JsoupUtils.selectFirst("PARUTION_BOAMP", document);
                    final Element rootElementCopy = rootElement.clone();
                    // we want just root element
                    rootElementCopy.children().remove();
                    rootElementCopy.appendChild(publicationElementCopy);
                    assert rootElementCopy.children().size() == 1;

                    final String sourceId = JsoupUtils.selectText("GESTION > NOJO", publicationElement);

                    mergeAndSave(rootElementCopy.toString(), ftpRecord.getSourceUrl(), fileName, sourceId);

                    if (++counter % PAGE_SIZE == 0) {
                        commit(counter, ftpRawId);
                    }
                }
            }
        }

        commit(counter, null);

        logger.info("FTP raw data processing finished, waiting for next work...");
    }

    @Override
    protected void resend(final String version, final String dateFrom, final String dateTo) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return JdbcTransactionUtils.getInstance();
    }

    /**
     * Merges FTP and web raw data and saves them to new record.
     *
     * @param sourceData
     *         FTP source data
     * @param sourceUrl
     *         FTP source URL
     * @param sourceFileName
     *         FTP source file name
     * @param publicationSourceId
     *         publication source ID. Web URL from the ID is created and we can match the records.
     */
    private void mergeAndSave(final String sourceData, final URL sourceUrl, final String sourceFileName,
                              final String publicationSourceId) {
        final String webPublicationUrl = String.format(BOAMPTenderUtils.PUBLICATION_PERMALINK_PATTERN,
                publicationSourceId);

        // get corresponding web publication
        final Raw webRecord = rawDAO.getBySourceUrl("eu.datlab.worker.fr.raw.BOAMPWebTenderDownloader", "1",
                webPublicationUrl);

        // merge them into new record (new record has valid id)
        final RawData newRecord = rawDAO.getEmptyInstance();
        // FTP record:
        newRecord.setSourceData(sourceData);
        newRecord.setSourceUrl(sourceUrl);
        newRecord.setSourceFileName(sourceFileName);
        // web record:
        final HashMap<String, Object> metaData = new HashMap<>();
        metaData.put(HTML_SOURCE_URL_METADATA_KEY, webRecord.getSourceUrl());
        metaData.put(HTML_SOURCE_DATA_METADATA_KEY, webRecord.getSourceData());
        newRecord.setMetaData(metaData);
        rawDAO.save(newRecord);
    }

    /**
     * Commits the transaction.
     *
     * @param counter
     *         number of new records which are saved
     * @param ftpRawId
     *         ID of FTP record
     */
    private void commit(final Integer counter, final String ftpRawId) {
        JdbcTransactionUtils.getInstance().commit();
        logger.info("Processed {} FTP raw data. Last ID is {}", counter, ftpRawId);
    }
}
