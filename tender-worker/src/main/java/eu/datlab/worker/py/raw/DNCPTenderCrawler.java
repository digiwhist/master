package eu.datlab.worker.py.raw;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.py.DNCPFormType;
import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;
import eu.dl.core.UnrecoverableException;
import java.io.File;
import java.io.FileFilter;

import java.io.IOException;
import java.net.URL;
import java.nio.charset.Charset;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.commons.lang3.StringUtils;

/**
 * Tender crawler for Paraguay.
 *
 * @author Tomas Mrazek
 */
public final class DNCPTenderCrawler extends BaseDatlabIncrementalCrawler {
    private static final String VERSION = "1.0";

    private static final String SOURCE_DOMAIN = PublicationSources.PY_DNCP;
    
    private static final int API_PORT = 443;

    private static final String BASE_API_URL = SOURCE_DOMAIN + ":" + API_PORT + "/datos/api/v2/doc/ocds/";

    private static final LocalDate DEFAULT_START_DATE = LocalDate.of(2010, Month.JANUARY, 1);

    private static final String BASE_CSV_URL = PublicationSources.PY_DNCP + "/images/opendata/";

    private static final String WORK_DIR = "PY_workFolder";

    private static final int CONNECTION_TIMEOUT = 60000;

    private static final int READ_TIMEOUT = 30000;

    private static final String BIDDERS_CSV_URL = "https://www.contrataciones.gov.py/licitaciones/adjudicacion/%s/oferentes.csv";

    /**
     * CSV parser.
     */
    private static final CSVFormat PARSER_FORMAT = CSVFormat.DEFAULT.withHeader().withSkipHeaderRecord();

    /**
     * Default constructor.
     */
    public DNCPTenderCrawler() {
        super();

        // create work folder if necessary
        final File destinationFolder = new File(FilenameUtils.getName(WORK_DIR));
        destinationFolder.mkdir();
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        int year = date.getYear();

        for (DNCPFormType type : DNCPFormType.values()) {
            if (type == DNCPFormType.RECORD_PACKAGE) {
                continue;
            }

            String csvPath = WORK_DIR + "/" + type.name().toLowerCase() + "_" + year;
            String csvUrl = getSourceCSVUrl(type, year);

            File csv = new File(csvPath);
            if (!csv.exists()) {
                // removes old files of the given type
                FileFilter oldFilesFilter = new WildcardFileFilter(type.name().toLowerCase() + "_*");
                File[] oldFiles = new File(WORK_DIR).listFiles(oldFilesFilter);
                for (File f : oldFiles) {
                    f.delete();
                }

                // download new file
                try {
                    logger.info("Downloading of {} CSV from {} starts", type, csvUrl);
                    FileUtils.copyURLToFile(new URL(csvUrl), new File(csvPath), CONNECTION_TIMEOUT, READ_TIMEOUT);
                } catch (IOException ex) {
                    logger.error("Unable to download CSV file from {}", csvUrl, ex);
                    throw new UnrecoverableException("Unable to download CSV file");
                }
            }

            try {                
                CSVParser parser = CSVParser.parse(csv, Charset.forName("UTF8"), PARSER_FORMAT);
                for (CSVRecord r : parser) {
                    // first ten characters should be date in ISO format
                    String published = StringUtils.substring(r.get(getFormTypePublicationDateColumn(type)), 0, 10);

                    try {
                        // parse only rows for the given date
                        if (published == null || published.isEmpty()
                            || !date.equals(LocalDate.parse(published, DateTimeFormatter.ofPattern("uuuu-MM-dd")))) {
                            continue;
                        }

                        String id = r.get("id");

                        String url = getEnpointUrl(type, id);

                        HashMap<String, Object> metaData = new HashMap<>();
                        metaData.put("formType", type.name());
                        metaData.put("publicationDate", published);
                        // form type specific metadata
                        metaData.putAll(getFormTypeMetaData(type, r));

                        // for award could be presented list of bidders
                        if (type == DNCPFormType.AWARD) {
                            metaData.put("bidders", String.format(BIDDERS_CSV_URL, id));
                        }

                        createAndPublishMessage(url, metaData);
                    } catch (DateTimeParseException ex) {
                        logger.warn("Unable to parse date of publication from {}", published, ex);
                    }
                }
            } catch (IOException ex) {
                logger.error("Unable to parse CSV file from {}", csvUrl, ex);
                throw new UnrecoverableException("Unable to parse CSV file");
            }
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected void finalCleanup() {
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return DEFAULT_START_DATE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    @Override
    protected void initialSetup() {
    }

    /**
     * @param formType
     *      form type
     * @param id
     *      id of the requested record
     *
     * @return url of an endpoint
     */
    private String getEnpointUrl(final DNCPFormType formType, final String id) {
        String endpoint = null;
        switch (formType) {
            case AMENDMENT:
                endpoint = "amendmment";
                break;
            case RECORD_PACKAGE:
                endpoint = "record-package";
                break;
            case AWARD:
            case CONTRACT:
            case PLANNING:
            case TENDER:
                endpoint = formType.name().toLowerCase();
                break;
            default:
                logger.error("Unsupported DNCP form type {}", formType);
                throw new UnrecoverableException("Unsupported DNCP form type");
        }

        return BASE_API_URL + endpoint + "/" + id;
    }

    /**
     * @param formType
     *      form type
     * @param record
     *      csv record
     * @return form type specific metaData or empty map
     */
    private Map<String, Object> getFormTypeMetaData(final DNCPFormType formType, final CSVRecord record) {
        if (formType == null) {
            return Collections.emptyMap();
        }

        switch (formType) {
            case AMENDMENT:
                return getCSVRecordMetaData(record, "descripcion", "vigencia_contrato", "fecha_contrato");
            case CONTRACT:
                return getCSVRecordMetaData(record, "tipo_procedimiento_id", "tipo_procedimiento_codigo", "tipo_procedimiento");
            case AWARD:
                return getCSVRecordMetaData(record, "tipo_procedimiento_id", "tipo_procedimiento_codigo", "tipo_procedimiento",
                    "organismo_financiador");
            case TENDER:
                return getCSVRecordMetaData(record, "fecha_publicacion", "fuente_financiamiento", "lugar_entrega_oferta");
            case PLANNING:
            case RECORD_PACKAGE:
                // no form type specific metadata
                return Collections.emptyMap();
            default:
                logger.error("Unsupported DNCP form type {}", formType);
                throw new UnrecoverableException("Unsupported DNCP form type");
        }
    }

    /**
     * @param record
     *      CSV record
     * @param fields
     *      list of fields to be parsed to metadata
     * @return metadata
     */
    private Map<String, Object> getCSVRecordMetaData(final CSVRecord record, final String... fields) {
        Map<String, Object> metaData = new HashMap<>();
        for (String f : fields) {
            metaData.put(f, record.get(f));
        }

        return metaData;
    }

    /**
     * @param formType
     *      form type
     * @param year
     *      year
     * @return URL of CSV file
     */
    private String getSourceCSVUrl(final DNCPFormType formType, final int year) {
        String endpoint = null;
        switch (formType) {
            case AMENDMENT:
                endpoint = "modificacion_contrato";
                break;
            case AWARD:
                endpoint = "adjudicaciones";
                break;
            case CONTRACT:
                endpoint = "contratos";
                break;
            case PLANNING:
                endpoint = "planificaciones";
                break;
            case TENDER:
                endpoint = "convocatorias";
                break;
            default:
                logger.error("Unsupported DNCP form type {}", formType);
                throw new UnrecoverableException("Unsupported DNCP form type");
        }

        return BASE_CSV_URL + endpoint + "/" + year + ".csv";
    }

    /**
     * @param type
     *      DNCP form type
     * @return name of column that holds publication date
     */
    private String getFormTypePublicationDateColumn(final DNCPFormType type) {
        switch (type) {
            case AMENDMENT:
                return "fecha_emision_cc";
            case CONTRACT:
                return "fecha_firma_contrato";
            default:
                return "fecha_publicacion";
        }
    }
}
