package eu.digiwhist.worker.master.statistic;

import java.util.Arrays;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * This worker calculates a value which indicates whether we shall pass particular tender to opentender.eu portal or
 * not.
 *
 * @author Tomas Mrazek
 */
public final class OpenTenderWorker extends BaseWorker {

    private static final String INCOMING_EXCHANGE_NAME = "master";

    private static final String OUTGOING_EXCHANGE_NAME = "master";

    private static final String VERSION = "1.0";

    private static TransactionUtils transactionUtils;

    private static MasterTenderDAO masterDao;

    protected static final List<String> COUNTRIES = Arrays.asList("DE", "IT", "SE", "BE", "FI", "AT", "DK", "GR", "LU",
        "CY", "MT", "IS", "RS", "AM");

    protected static final BigDecimal THRESHOLD = new BigDecimal(135000);

    protected static final String WORKER_PL = "eu.digiwhist.worker.pl.master.UZPTenderMaster";
    protected static final String WORKER_EU = "eu.digiwhist.worker.eu.master.TedTenderMaster";
    protected static final String WORKER_GE = "eu.digiwhist.worker.ge.master.SPATenderMaster";

    /**
     * Default constructor with initialization of everything.
     */
    public OpenTenderWorker() {
        super();
        transactionUtils = DAOFactory.getDAOFactory().getTransactionUtils();
        masterDao = DAOFactory.getDAOFactory().getMasterTenderDAO(getName(), VERSION);
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
        transactionUtils.begin();
    		
        String id = message.getValue("id");

        final MasterTender tender = masterDao.getById(id);

        if (tender != null) {
            HashMap<String, Object> metaData = Optional.ofNullable(tender.getMetaData()).orElse(new HashMap<>());

            // do not overwrite already calculated
            if (metaData.containsKey("opentender")) {
                return;
            }

            metaData.put("opentender", isOpenTender(tender));
            tender.setMetaData(metaData);
            masterDao.save(tender);            
        }

        transactionUtils.commit();
    }

    @Override
    protected void resend(final String version, final String dateFrom, final String dateTo) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    /**
     * Checks whether the given tender is suitable for opentender export acording to folowing rules.
     * <ul>
     *      <li>if tender.createdBy = "eu.digiwhist.worker.eu.master.TedTenderMaster" and tender.country in
     *      {@link OpenTenderWorker#COUNTRIES} then <b>TRUE</b></li>
     *      <li>if tender.createdBy = "eu.digiwhist.worker.pl.master.UZPTenderMaster" or
     *      "eu.digiwhist.worker.ge.master.SPATenderMaster" then <b>TRUE</b></li>
     *      <li>if tender.createdBy = "eu.digiwhist.worker.eu.master.TedTenderMaster" and price > 135000 EUR
     *      ({@link OpenTenderWorker#getPrice(eu.dl.dataaccess.dto.master.MasterTender)}) and tender.country not in
     *      {@link OpenTenderWorker#COUNTRIES} then <b>TRUE</b></li>
     *      <li>if tender.createdBy not in ("eu.digiwhist.worker.eu.master.TedTenderMaster",
     *      "eu.digiwhist.worker.pl.master.UZPTenderMaster", "eu.digiwhist.worker.ge.master.SPATenderMaster")
     *      and price {@code <=} 135000 EUR then <b>TRUE</b></li>
     *      <li>price = null then <b>TRUE</b></li>
     *      <li>otherwise <b>FALSE</b></li>
     * </ul>
     * 
     * @param tender
     *      master tender to parse from
     * @return true only and only if the tender is suitable for open tender export, otherwise false.
     */
    protected static boolean isOpenTender(final MasterTender tender) {
        if (tender == null || tender.getCreatedBy() == null) {
            return false;
        }

        if ((tender.getCreatedBy().equals(WORKER_EU) && COUNTRIES.contains(tender.getCountry()))
            || tender.getCreatedBy().equals(WORKER_PL)
            || tender.getCreatedBy().equals(WORKER_GE)) {

            return true;
        } else {
            BigDecimal price = getPrice(tender);

            if (price == null
                || (tender.getCreatedBy().equals(WORKER_EU) && !COUNTRIES.contains(tender.getCountry())
                    && price.compareTo(THRESHOLD) > 0)
                || (!Arrays.asList(WORKER_EU, WORKER_GE, WORKER_PL).contains(tender.getCreatedBy()))
                    && price.compareTo(THRESHOLD) <= 0) {

                return true;
            }
        }

        return false;
    }

    /**
     * Returns tender price acording to following rules.
     *
     * <ul>
     *      <li>if is not null return tender.finalPrice.netAmountEur</li>
     *      <li>else if is not null return tender.estimatedPrice.netAmountEur</li>
     *      <li>else if not null return sum(lot[i].bid[isWinning=true].price.netAmountEur)</li>
     *      <li>else if not null return sum(lot[i].estimatedPrice.netAmountEur)</li>
     *      <li>else return null</li>
     * </ul>
     *
     * @param tender
     *      master tender to parse from
     * @return price of the tender or null
     */
    protected static BigDecimal getPrice(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

        if (tender.getFinalPrice() != null && tender.getFinalPrice().getNetAmountEur() != null) {
            return tender.getFinalPrice().getNetAmountEur();
        } else if (tender.getEstimatedPrice()!= null && tender.getEstimatedPrice().getNetAmountEur() != null) {
            return tender.getEstimatedPrice().getNetAmountEur();
        }


        if (tender.getLots() != null) {
            // sum of the net amount prices in euros of winning bids of all lots.
            Supplier<Stream<BigDecimal>> sum = () -> tender.getLots().stream()
                .flatMap(n -> {
                    if (n.getBids() == null) {
                        return Stream.empty();
                    }
                    
                    // get all non-null net amount prices in euros of winning bids.
                    return n.getBids().stream()
                        .filter(m -> Objects.equals(m.getIsWinning(), Boolean.TRUE) && m.getPrice() != null
                            &&  m.getPrice().getNetAmountEur() != null)
                        .map(m -> m.getPrice().getNetAmountEur());
                });

            // sum of the net amount estimated prices in euros of all lots.
            if (sum.get().count() == 0) {
                sum = () -> tender.getLots().stream()
                    .filter(n -> n.getEstimatedPrice() != null && n.getEstimatedPrice().getNetAmountEur() != null)
                    .map(n -> n.getEstimatedPrice().getNetAmountEur());
            }
            

            return sum.get().count() == 0 ? null : sum.get().reduce(BigDecimal.ZERO, BigDecimal::add);
        }

        return null;
    }
}
