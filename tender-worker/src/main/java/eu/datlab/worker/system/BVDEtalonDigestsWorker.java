package eu.datlab.worker.system;

import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import eu.datlab.dataaccess.dao.jdbc.JdbcBVDEtalonBodyDAO;
import eu.datlab.dataaccess.dto.matched.BVDEtalonBody;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dao.jdbc.JdbcTransactionUtils;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.utils.DigestUtils;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.clean.utils.StringUtils;

/**
 * This worker generates standardizedname, standardizedaddress, digests for BvD
 * etalons that have any of these fields null.
 * 
 * @author Tomas Mrazek
 */
public final class BVDEtalonDigestsWorker extends BaseWorker {
    
    private static final String INCOMING_EXCHANGE_NAME = "init";

    private static final String OUTGOING_EXCHANGE_NAME = "system";
    
    private static final String VERSION = "1.0";
    
    private EtalonBodyDAO<BVDEtalonBody> etalonBodyDao;

    private static final Integer PAGE_SIZE = 50000;

    /**
     * Initialization of everythong.
     */
    public BVDEtalonDigestsWorker() {
        super();
        // handle commits manually, disable autocmmit feature
        try {
            JdbcTransactionUtils.getInstance().getConnection().setAutoCommit(false);
        } catch (SQLException e) {
            logger.debug("Unable to disable autocommit {}.", e);
            throw new UnrecoverableException("Unable to disable autocommit", e);
        }
        etalonBodyDao = new JdbcBVDEtalonBodyDAO();
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
        Integer startId = Integer.valueOf(message.getValue("startId"));
        Integer endId = Integer.valueOf(message.getValue("endId"));
        
        logger.error("Process bodies with id >= {}", startId);
        Integer actualId = startId;
        
        do {
            // get bodies to be processd
            List<BVDEtalonBody> bodies = etalonBodyDao.findAllById(actualId, PAGE_SIZE);
            if (bodies.isEmpty()) {
                logger.error("No bodies found, lets end the loop.");
                // end the loop execution as everything has been done already
                break;
            }
            logger.debug("Processing {} bodies", bodies.size());
            // go through the result set and update entries
            for (BVDEtalonBody etalon : bodies) {
                MatchedBody matchedEtalon = etalon.getAsMatchedBody();
                Address addr = matchedEtalon.getAddress();
                etalon
                    // data cleaninig
                    .setEuropeanVatNumber(StringUtils.cleanShortString(etalon.getEuropeanVatNumber()))
                    .setStatisticalNumber(StringUtils.cleanShortString(etalon.getStatisticalNumber()))
                    .setTradeRegisterNumber(StringUtils.cleanShortString(etalon.getTradeRegisterNumber()))
                    .setVatTaxNumber(StringUtils.cleanShortString(etalon.getVatTaxNumber()))
                    .setNuts3((addr != null && addr.getNuts() != null) ? addr.getNuts().get(0) : null)
                    // digests calculation
                    .setStandardizedName(DigestUtils.standardizeName(matchedEtalon.getName()))
                    .setStandardizedAddress(DigestUtils.standardizeAddress(matchedEtalon.getAddress()))
                    .setDigest(DigestUtils.digest(matchedEtalon))
                    .setDigest2(digest2(matchedEtalon));

                // data updates
                etalonBodyDao.updateDigestsAndBodyIdsAndNuts(etalon);

                if (etalon.getDigest() == null) {
                    logger.error("Digest is null for id {}", etalon.getId());
                }
                
                actualId = Integer.valueOf(etalon.getId());
            }

            JdbcTransactionUtils.getInstance().commit();

            // increase page size
            logger.error("Processed bodies until id {}.", actualId);

        } while (actualId < endId);

        logger.error("Bodies processing finished, waiting for next work...");
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
     * Returns combination of digests for standardized name and address. Both values are separated by a
     * DigestUtils.SEPARATOR.
     *
     * @see #digestName(java.lang.String)
     * @see DigestUtils#digestAddress(java.lang.String)
     *
     * @param standardizedName
     *      standardized name that needs digest
     * @param standardizedAddress
     *      standardized address that needs digest
     * @return combined digest such a name_digestSEPARATORaddress_digest
     */
    protected static String digest2(final String standardizedName, final String standardizedAddress) {
        String digName = digestName(standardizedName);
        String digAddr = DigestUtils.digestAddress(standardizedAddress);

        StringBuilder sb = new StringBuilder();
        if (digName != null) {
            sb.append(digName);
        }

        sb.append(DigestUtils.SEPARATOR);

        if (digAddr != null) {
            sb.append(digAddr);
        }

        return (sb.length() == 0 || sb.toString().equals(DigestUtils.SEPARATOR)) ? null : sb.toString();
    }

    /**
     * Returns combination of digests for the given matched {@code body}. In case that standardized address is null for
     * the address of this body, attempts to compose raw address and uses it's stadardized form in digest. Raw address
     * must includes at least two not null parameters from following four street, city, postcode, and country.
     *
     * @see #digest2(java.lang.String, java.lang.String)
     * @see DigestUtils#standardizeAddress(eu.dl.dataaccess.dto.generic.Address)
     *
     * @param body
     *      matched body
     * @return combined digest such a name_digestSEPARATORaddress_digest
     */
    protected static String digest2(final MatchedBody body) {
        if (body == null) {
            return null;
        }

        Address adr = body.getAddress();
        String stdName = DigestUtils.standardizeName(body.getName());
        String digest = digest2(stdName, DigestUtils.standardizeAddress(adr));

        if ((digest == null || digest.endsWith("|")) && adr != null) {
            List<String> fields = Arrays.asList(adr.getStreet(), adr.getCity(), adr.getPostcode(), adr.getCountry());

            if (fields.stream().mapToInt(n -> n == null ? 0 : 1).sum() >= 2) {
                String rawAddr = fields.stream().reduce("", (r, n) -> r + Optional.ofNullable(n).orElse(""));

                return digest2(stdName, DigestUtils.standardizeAddress(new Address().setRawAddress(rawAddr)));
            }
        }

        return digest;
    }

    /**
     * Returns first two alphanumerical symbols in a standardized name (turned into lowercase).
     *
     * @param stadardizedName
     *      standardized name that needs digest
     * @return digest of the name or null
     */
    private static String digestName(final String stadardizedName) {
        if (stadardizedName == null) {
            return null;
        }

        //matches arbitrary alphanumerical character
        final String digest = DigestUtils.digestString(stadardizedName.toLowerCase(), "[\\p{L}\\d]", 2);

        return (digest.length() == 2 ? digest : null);
    }
}
