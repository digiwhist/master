package eu.datlab.worker.master.statistic;

import java.util.Arrays;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import java.math.BigDecimal;
import java.util.ArrayList;
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
    protected static final BigDecimal THRESHOLD_WORKS = new BigDecimal(5186000);

    protected static final String WORKER_PL = "eu.datlab.worker.pl.master.UZPTenderMaster";    
    protected static final String WORKER_GE = "eu.datlab.worker.ge.master.SPATenderMaster";
    protected static final List<String> WORKERS_EU = Arrays.asList("eu.datlab.worker.eu.master.TedTenderMaster",
        "eu.datlab.worker.eu.master.TedCSVTenderMaster");

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
     * Checks whether the given tender is suitable for opentender export.
     * 
     * @param tender
     *      master tender to parse from
     * @return true only and only if the tender is suitable for open tender export, otherwise false.
     */
    protected static boolean isOpenTender(final MasterTender tender) {
        if (tender == null || tender.getCreatedBy() == null) {
            return false;
        }

        if ((WORKERS_EU.contains(tender.getCreatedBy()) && COUNTRIES.contains(tender.getCountry()))
            || tender.getCreatedBy().equals(WORKER_PL)
            || tender.getCreatedBy().equals(WORKER_GE)) {

            return true;
        } else {
            BigDecimal price = getPrice(tender);

            List<String> workers = new ArrayList<>(WORKERS_EU);
            workers.add(WORKER_GE);
            workers.add(WORKER_PL);

            BigDecimal threshold = isWorks(tender) ? THRESHOLD_WORKS : THRESHOLD;

            if (price == null
                || (WORKERS_EU.contains(tender.getCreatedBy()) && !COUNTRIES.contains(tender.getCountry())
                    && price.compareTo(threshold) > 0)
                || (!workers.contains(tender.getCreatedBy()) && price.compareTo(threshold) <= 0)) {

                return true;
            }
        }

        return false;
    }

    /**
     * @param tender
     *      master tender
     * @return TRUE if the given tender is WORKS
     */
    private static boolean isWorks(final MasterTender tender) {
        if (tender == null) {
            return false;
        }

        if (tender.getSupplyType() != null) {
            return  TenderSupplyType.WORKS == tender.getSupplyType();
        } else {
            boolean tenderIsWorks = tender.getCpvs() != null && tender.getCpvs().stream()
                .filter(n -> Boolean.TRUE.equals(n.getIsMain()))
                .findFirst().map(OpenTenderWorker::isWorksCPV).orElse(false);

            boolean lotIsWorks = tender.getLots() != null && tender.getLots().stream()
                .map(MasterTenderLot::getCpvs).filter(Objects::nonNull).flatMap(n -> n.stream())
                .filter(n -> Boolean.TRUE.equals(n.getIsMain())).findFirst().map(OpenTenderWorker::isWorksCPV).orElse(false);

            return tenderIsWorks || lotIsWorks;
        }
    }

    /**
     * @param cpv
     *      cpv
     * @return TRUE in case that CVP code starts with 45, otherwise FALSE
     */
    private static boolean isWorksCPV(final CPV cpv) {
        return cpv == null || cpv.getCode() == null ? false : cpv.getCode().startsWith("45");
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
