package eu.dl.worker.master.utils;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSize;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.utils.currency.CurrencyService;
import eu.dl.utils.currency.CurrencyServiceFactory;
import org.apache.commons.lang.StringUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Currency;
import java.util.HashSet;
import java.util.Set;

/**
 * Utility class used to handle tender size.
 */
public final class TenderSizeUtils {
    /**
     * Default constructor.
     */
    protected TenderSizeUtils() {
        // utility class
    }

    /**
     * Application config instance.
     */
    private static final Config config = Config.getInstance();

    /**
     * Calculates and sets tender size for given tender.
     *
     * @param tender master tender
     *
     * @return new tender size
     */
    public static TenderSize calculate(final MasterTender tender) {
        // check prerequisities
        if (!checkTender(tender)) {
            return null;
        }

        LocalDate date = pickConversionDate(tender);
        if (date == null) {
            return null;
        }

        LocalDate firstDayOfPeriod = getPeriodDate(date);

        BigDecimal tenderPrice = getTenderPrice(tender);

        if (tenderPrice == null) {
            return null;
        }

        String configKey = "tenderSize.threshold";

        configKey += "." + getBuyerTypeKey(tender);
        configKey += "." + getSupplyTypeKey(tender);
        configKey += "." + firstDayOfPeriod.getYear();

        Currency nationalCurrency = getNationalCurrency(tender);
        if (nationalCurrency == null) {
            return null;
        }

        BigDecimal threshold = convert(
                new BigDecimal(config.getParam(configKey)),
                nationalCurrency,
                firstDayOfPeriod);


        if (threshold.multiply(new BigDecimal(1.02)).compareTo(tenderPrice) == -1) {
            return TenderSize.ABOVE_THE_THRESHOLD;
        } else {
            return TenderSize.BELOW_THE_THRESHOLD;
        }
    }

    /**
     * Searches in prices for currency national value.
     *
     * @param tender searches in this tender
     *
     * @return national currency, null if nothing found
     */
    private static Currency getNationalCurrency(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

        // tender.estimatedPrice
        if (tender.getEstimatedPrice() != null && tender.getEstimatedPrice().getCurrencyNational() != null) {
            return tender.getEstimatedPrice().getCurrencyNational();
        }

        // sum(lot.robustEstimatedPrice)
        // sum(lot.bid[winning].robustPrice)
        if (tender.getLots() != null) {
            BigDecimal lotSum = new BigDecimal(0);
            BigDecimal bidSum = new BigDecimal(0);

            for (MasterTenderLot lot : tender.getLots()) {
                if (lot.getRobustEstimatedPrice() != null
                        && lot.getRobustEstimatedPrice().getCurrencyNational() != null) {
                    return lot.getRobustEstimatedPrice().getCurrencyNational();
                }

                if (lot.getBids() != null) {
                    for (MasterBid bid: lot.getBids()) {
                        if (bid.getIsWinning()
                                && bid.getRobustPrice() != null
                                && bid.getRobustPrice().getCurrencyNational() != null) {
                            return bid.getRobustPrice().getCurrencyNational();
                        }
                    }
                }
            }
        }

        return null;
    }

    /**
     * Gets part of config key for buyer type.
     * @param tender tender
     * @return key
     */
    private static String getBuyerTypeKey(final MasterTender tender) {
        if (tender.getBuyers() != null) {
            for (MasterBody body : tender.getBuyers()) {
                BuyerType buyerType = body.getBuyerType();
                if (buyerType == BuyerType.NATIONAL_AGENCY || buyerType == BuyerType.NATIONAL_AUTHORITY) {
                    return "NATIONAL";
                }
                if (buyerType == BuyerType.REGIONAL_AGENCY || buyerType == BuyerType.REGIONAL_AUTHORITY) {
                    return "REGIONAL";
                }
            }
        }

        return "OTHERS";
    }

    /**
     * Gets part of config key for supply type.
     * @param tender tender
     * @return key
     */
    private static String getSupplyTypeKey(final MasterTender tender) {
        if (tender.getSupplyType() != null
                && (tender.getSupplyType() == TenderSupplyType.SUPPLIES
                || tender.getSupplyType() == TenderSupplyType.SERVICES)) {
            return "SUPPLIES_SERVICES";
        }

        return "WORKS";
    }

    /**
     * Chooses the first date of period.
     * @param date date
     * @return date
     */
    private static LocalDate getPeriodDate(final LocalDate date) {
        Integer year = date.getYear();
        if ((year % 2) == 1) {
            year--;
        }

        return LocalDate.of(year, 1, 1);
    }

    /**
     * Picks the date, for which should be the currency conversion performed.
     *
     * @param tender master tender
     * @return date
     */
    private static LocalDate pickConversionDate(final MasterTender tender) {
        if (tender == null || tender.getPublications() == null) {
            return null;
        }

        LocalDate minDate = null;
        LocalDate minContractAwardDate = null;
        LocalDate minNoticeDate = null;
        for (Publication publication : tender.getPublications()) {
            if (publication.getPublicationDate() != null) {
                if (minDate == null || minDate.isAfter(publication.getPublicationDate())) {
                    minDate = publication.getPublicationDate();
                }

                if (publication.getFormType() == PublicationFormType.CONTRACT_AWARD
                        && publication.getIsIncluded() != null && publication.getIsIncluded()) {
                    if (minContractAwardDate == null || minContractAwardDate.isAfter(
                            publication.getPublicationDate())) {
                        minContractAwardDate = publication.getPublicationDate();
                    }
                }

                if (publication.getFormType() == PublicationFormType.CONTRACT_NOTICE
                        && publication.getIsIncluded() != null && publication.getIsIncluded()) {
                    if (minNoticeDate == null || minNoticeDate.isAfter(
                            publication.getPublicationDate())) {
                        minNoticeDate = publication.getPublicationDate();
                    }
                }
            }
        }


        if (minNoticeDate != null) {
             return minNoticeDate;
        } else if (minContractAwardDate != null) {
            return minContractAwardDate;
        } else {
            return null;
        }
    }


    /**
     * Selects the price of the tender in EUR. Price is evaulated as (in order of preference)
     * tender.estimatedPrice, sum(lot.robustEstimatedPrice), sum(lot.bid[winning].robustPrice)
     *
     * @param tender tender
     *
     * @return price
     */
    private static BigDecimal getTenderPrice(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

        // tender.estimatedPrice
        if (tender.getEstimatedPrice() != null && tender.getEstimatedPrice().getNetAmountNational() != null) {
                    return tender.getEstimatedPrice().getNetAmountNational();
        }

        // sum(lot.robustEstimatedPrice)
        // sum(lot.bid[winning].robustPrice)
        if (tender.getLots() != null) {
            BigDecimal lotSum = new BigDecimal(0);
            BigDecimal bidSum = new BigDecimal(0);

            for (MasterTenderLot lot : tender.getLots()) {
                if (lot.getRobustEstimatedPrice() != null
                        && lot.getRobustEstimatedPrice().getNetAmountNational() != null) {
                    lotSum = lotSum.add(lot.getRobustEstimatedPrice().getNetAmountNational());
                }

                if (lot.getBids() != null) {
                    for (MasterBid bid: lot.getBids()) {
                        if (bid.getIsWinning()
                                && bid.getRobustPrice() != null
                                && bid.getRobustPrice().getNetAmountNational() != null) {
                            bidSum = bidSum.add(bid.getRobustPrice().getNetAmountNational());
                        }
                    }
                }
            }

            if (lotSum.compareTo(new BigDecimal(0)) != 0) {
                return lotSum;
            }

            if (bidSum.compareTo(new BigDecimal(0)) != 0) {
                return bidSum;
            }
        }

        return null;
    }

    /**
     * Converts EUR to currency provided.
     * @param price converted price
     * @param currency target currency
     * @param conversionDate conversion date
     * @return value in EUR
     */
    private static BigDecimal convert(final BigDecimal price, final Currency currency, final LocalDate conversionDate) {
        CurrencyService currencyService = CurrencyServiceFactory.getCurrencyService();
        return currencyService.convert(
                Currency.getInstance("EUR"),
                currency,
                price,
                conversionDate);
    }

    /**
     * Checks whether the tender size should be calculated or not.
     *
     * @param masterTender master tender
     *
     * @return true if tender size should be calculated
     */
    private static boolean checkTender(final MasterTender masterTender) {
        // We will NOT set the threshold for tenders with
        // 1) main CPV listed in ANNEX XIV here:
        // http://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX%3A32014L0024
        if (masterTender.getCpvs() != null && !masterTender.getCpvs().isEmpty()) {
            Set<String> rawOmittedCPVs = config.getParamValueAsList("tenderSiza.ommitCPVs", ",", HashSet.class);
            Set<String> omittedCPVs = unwind(rawOmittedCPVs);
            for (CPV cpv : masterTender.getCpvs()) {
                if (omittedCPVs.contains(StringUtils.substring(cpv.getCode(), 0, 5))) {
                    return false;
                }
            }
        }

        // 2) procerureType = DESIGN_CONTEST
        if (masterTender.getProcedureType() != null
                && masterTender.getProcedureType() == TenderProcedureType.DESIGN_CONTEST) {
            return false;
        }

        // 3) buyerActivity = DEFENCE
        if (masterTender.getBuyers() != null) {
            for (MasterBody buyer : masterTender.getBuyers()) {
                if (buyer.getMainActivities() != null) {
                    for (BuyerActivityType activity : buyer.getMainActivities()) {
                        if (activity == BuyerActivityType.DEFENCE) {
                            return false;
                        }
                    }
                }
            }
        }

        // 4) there is more than 5 lots
        if (masterTender.getLots() != null && masterTender.getLots().size() > 5) {
            return false;
        }

        return true;
    }

    /**
     * Unwinds definition of CPVs. For example 12345-12347 results into
     * 12345,12346,12347.
     *
     * @param rawCPVs definition of a omitted CPVS
     *
     * @return resulting set
     */
    private static Set<String> unwind(final Set<String> rawCPVs) {
        if (rawCPVs == null) {
            return null;
        }

        Set<String> result = new HashSet<>();

        for (String cpv : rawCPVs) {
            if (cpv.length() == 5) {
                result.add(cpv);
            } else if (cpv.length() ==11) {
                String[] range = cpv.split("-");
                Integer start = Integer.valueOf(range[0]);
                Integer end = Integer.valueOf(range[1]);

                while (start < end) {
                   result.add(start.toString());
                   start++;
                }

                result.add(end.toString());
            }
        }

        return result;
    }
}
