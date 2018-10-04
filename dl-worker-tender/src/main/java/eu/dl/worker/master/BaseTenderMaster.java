package eu.dl.worker.master;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.generic.BasePrice;
import eu.dl.dataaccess.dto.generic.Payment;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.generic.UnitPrice;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.utils.DTOUtils;
import eu.dl.utils.currency.CurrencyService;
import eu.dl.utils.currency.CurrencyServiceFactory;
import eu.dl.utils.currency.UnconvertableException;
import eu.dl.worker.master.plugin.specific.DigiwhistPricePlugin;
import eu.dl.worker.master.plugin.specific.NoLotStatusPlugin;
import eu.dl.worker.master.utils.ContractImplementationUtils;
import org.apache.commons.lang.StringUtils;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Currency;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Common functionality for all Tender Master Record Deduplicators.
 *
 * @param <T> tender type to be for matched
 * @param <V> tender type to be for master
 */
public abstract class BaseTenderMaster<T extends MatchedTender, V extends MasterTender> extends BaseMaster<T, V> {

    private static final int NUMBER_OF_LOTS_THRESHOLD = 10000;
    private static final int NUMBER_OF_PUBLICATIONS = 10000;

    private final CurrencyService currencyService = CurrencyServiceFactory.getCurrencyService();

    private final DigiwhistPricePlugin digiwhistPricePlugin;

    private final NoLotStatusPlugin noLotStatusPlugin;

    /**
     * Initialization of everything.
     */
    public BaseTenderMaster() {
        super();
        config.addConfigFile("indicator");

        digiwhistPricePlugin = new DigiwhistPricePlugin();

        noLotStatusPlugin = new NoLotStatusPlugin();
    }

    @Override
    protected final void registerCommonPlugins() {
    }

    @Override
    protected final List<T> generalPreprocessData(final List<T> items) {
        List preprocessedData = items.stream()
                .filter(isNotContractImplementation())
                .filter(isNotTypeOther())
                .filter(hasNotTooMuchLots())
                .collect(Collectors.toList());

        // add publication dates to TenderParts where needed i.e. Document
        populatePublicationDates(preprocessedData);

        if (preprocessedData.size() > NUMBER_OF_PUBLICATIONS) {
            return preprocessedData.subList(0, NUMBER_OF_PUBLICATIONS-1);
        }

        return preprocessedData;
    }

    /**
     * Add tender ids to TenderParts where needed i.e. Document.
     *
     * @param items items to be populated
     */
    private void populatePublicationDates(final List<T> items) {
        for (T item : items) {
            LocalDate publicationDate = DTOUtils.getPublicationDate(item);
            if (publicationDate != null) {
                List<MasterablePart> parts = new ArrayList<MasterablePart>();
                parts.add(item);

                if (item.getDocuments() != null) {
                    parts.addAll(item.getDocuments());
                }

                if (item.getDocumentsPrice() != null) {
                    parts.add(item.getDocumentsPrice());
                }

                if (item.getFinalPrice() != null) {
                    parts.add(item.getFinalPrice());
                }

                if (item.getEstimatedPrice() != null) {
                    parts.add(item.getEstimatedPrice());
                }

                if (item.getEstimatedPrice() != null) {
                    parts.add(item.getFinalPrice());
                }

                if (item.getLots() != null) {
                    for (MatchedTenderLot lot : item.getLots()) {
                        parts.addAll(item.getLots());
                        if (lot != null) {
                            if (lot.getEstimatedPrice() != null) {
                                parts.add(lot.getEstimatedPrice());
                            }

                            if (lot.getBids() != null) {
                                parts.addAll(lot.getBids());
                                for (MatchedBid bid : lot.getBids()) {
                                    if (bid != null) {
                                        if (bid.getUnitPrices() != null) {
                                            parts.addAll(bid.getUnitPrices());
                                        }

                                        if (bid.getSubcontractedValue() != null) {
                                            parts.add(bid.getSubcontractedValue());
                                        }

                                        if (bid.getPrice() != null) {
                                            parts.add(bid.getPrice());
                                        }

                                        if (bid.getRobustPrice() != null) {
                                            parts.add(bid.getRobustPrice());
                                        }

                                        if (bid.getDocuments() != null) {
                                            parts.addAll(bid.getDocuments());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                for (MasterablePart part : parts) {
                    if (part != null) {
                        part.setPublicationDate(publicationDate);
                    }
                }
            }
        }
    }

    /**
     * Predicate used to filter the resulting set of items. In this case the group can not be Contract Implementation.
     *
     * @return predicate testing whether the group does not contain Contract Implementation
     */
    private static Predicate<MatchedTender> isNotContractImplementation() {
        return new Predicate<MatchedTender>() {

            @Override
            public boolean test(final MatchedTender t) {
                for (Publication publication : t.getPublications()) {
                    if (publication.getIsIncluded() != null && publication.getIsIncluded()
                            && publication.getFormType() != null
                            && publication.getFormType().equals(PublicationFormType.CONTRACT_IMPLEMENTATION)) {
                        return false;
                    }
                }
                return true;
            }

        };
    }

    /**
     * Predicate used to filter the resulting set of items. In this case the tender can not be Contract Implementation.
     *
     * @return predicate testing whether the group does not contain Contract
     */
    protected static Predicate<MatchedTender> isNotContractAmendment() {
        return new Predicate<MatchedTender>() {

            @Override
            public boolean test(final MatchedTender t) {
                for (Publication publication : t.getPublications()) {
                    if (publication.getIsIncluded() != null && publication.getIsIncluded()
                            && publication.getFormType() != null
                            && publication.getFormType().equals(PublicationFormType.CONTRACT_AMENDMENT)) {
                        return false;
                    }
                }
                return true;
            }

        };
    }

    /**
     * Predicate used to filter the resulting set of items. In this case the group should be Contract Implementation.
     *
     * @return predicate testing whether the group does contain Contract Implementation
     */
    private static Predicate<MatchedTender> isContractImplementation() {
        return new Predicate<MatchedTender>() {

            @Override
            public boolean test(final MatchedTender t) {
                for (Publication publication : t.getPublications()) {
                    if (publication.getIsIncluded() != null && publication.getIsIncluded()
                            && publication.getFormType() != null
                            && publication.getFormType().equals(PublicationFormType.CONTRACT_IMPLEMENTATION)) {
                        return true;
                    }
                }
                return false;
            }

        };
    }

    @Override
    protected final String getPersistentId(final List<T> matchedItems) {
        String persistentId = null;
        LocalDate min = null;
        
        for (T t : matchedItems) {
            for (Publication publication : t.getPublications()) {
                if (publication.getIsIncluded() != null && publication.getIsIncluded()) {
                    if ((min == null)
                            || (publication.getPublicationDate() != null
                            && min.isAfter(publication.getPublicationDate()))) {
                        min = publication.getPublicationDate();
                        persistentId = t.getPersistentId();
                    }
                }
            }
        }
        
        return persistentId;
    }
    
    /**
     * Predicate used to filter the resulting set of items. It does skip tenders 
     * with publication type = OTHER and isIncluded = true.
     *
     * @return predicate used to test type and isIncluded of tender.
     */
    private static Predicate<MatchedTender> isNotTypeOther() {
        return new Predicate<MatchedTender>() {

            @Override
            public boolean test(final MatchedTender t) {
                for (Publication publication : t.getPublications()) {
                    if (publication.getIsIncluded() != null 
                            && publication.getIsIncluded()
                            && publication.getFormType() != null
                            && publication.getFormType().equals(PublicationFormType.OTHER)) {
                        
                        return false;
                    }
                }
                return true;
            }

        };
    }
    
    /**
     * Predicate used to filter the resulting set of items. In this case we don't want to master a tender 
     * with too much lots because of performance issues.
     *
     * @return predicate testing whether the group does not contain too much lots
     */
    // TODO[Hruby] - get rid of this quick fix and make algorithm more effective
    private Predicate<MatchedTender> hasNotTooMuchLots() {
        return new Predicate<MatchedTender>() {

            @Override
            public boolean test(final MatchedTender t) {
                if (t.getLots() != null && t.getLots().size() < NUMBER_OF_LOTS_THRESHOLD) {
                    return true;
                } else {
                    return t.getLots() == null;
                }
            }

        };
    }

    @Override
    protected final V postProcessMasterRecord(final V masterTender,
            final List<T> matchedTenders) {
        processContractImplementation(masterTender, matchedTenders);

        updateLotStatus(masterTender);
        
        convertPrices(masterTender);

        MasterTender tender = masterTender;
        tender = digiwhistPricePlugin.master(null, tender, null);
        tender = noLotStatusPlugin.master(null, tender, null);

        return (V) tender;
    }
    
    /**
     * This method sets status of each lot to CANCELLED if tender.isWholeTenderCancelled is TRUE.
     *
     * @param tender lots are updated for this tender
     */
    private void updateLotStatus(final MasterTender tender) {
        if (tender == null || tender.getLots() == null) {
            return;
        }

        // lot status update for whole cancelled tender
        if (Boolean.TRUE.equals(tender.getIsWholeTenderCancelled())) {
            tender.getLots().forEach(l -> l.setStatus(TenderLotStatus.CANCELLED));
        }
    }

    /**
     * This method updates master tender with data about contract implementations. There are payments
     * mastered including the contract implementations skipped in previous phase. Additionaly, the publication
     * info is added to set of publications.
     *
     * @param masterTender master tender
     * @param matchedTenders matched tenders
     */
    private void processContractImplementation(final V masterTender, final List<T> matchedTenders) {
        List<MatchedTender> contractImplementations = matchedTenders.stream().filter(isContractImplementation())
                .collect(Collectors.toList());

        ContractImplementationUtils.addPaymentsFromContractImplementations(masterTender, contractImplementations);
    }


    /**
     * This method converts all prices to national currency and EUR where available.
     * 
     * @param tender master tender
     */
    private void convertPrices(final MasterTender tender) {
        LocalDate conversionDate = pickConversionDate(tender);
        if (conversionDate == null) {
            // no publications found, unable to convert
            logger.info("There is no usable publication date found. Prices will not be converted.");
            return;
        }
        convertPrice(tender.getEstimatedPrice(), conversionDate);
        convertPrice(tender.getDocumentsPrice(), conversionDate);
        convertPrice(tender.getFinalPrice(), conversionDate);
        if (tender.getLots() != null) {
            for (MasterTenderLot lot : tender.getLots()) {
                convertPrice(lot.getRobustEstimatedPrice(), conversionDate);
                convertPrice(lot.getEstimatedPrice(), conversionDate);
                if (lot.getBids() != null) {
                    for (MasterBid bid : lot.getBids()) {
                        convertPrice(bid.getPrice(), conversionDate);
                        convertPrice(bid.getRobustPrice(), conversionDate);

                        if (bid.getPayments() != null) {
                            for (Payment payment : bid.getPayments()) {
                                convertPrice(payment.getPrice(), conversionDate);
                            }
                        }

                        if (bid.getUnitPrices() != null) {
                            for (UnitPrice unitPrice : bid.getUnitPrices()) {
                                convertPrice(unitPrice, conversionDate);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Picks the date, for which should be the currency conversion performed.
     * 
     * @param tender master tender
     * @return date
     */
    private LocalDate pickConversionDate(final MasterTender tender) {
        if (tender == null) {
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
                
                if (publication.getFormType() == PublicationFormType.CONTRACT_AWARD  && publication.getIsIncluded()) {
                    if (minContractAwardDate == null || minContractAwardDate.isAfter(
                            publication.getPublicationDate())) {
                        minContractAwardDate = publication.getPublicationDate();
                    } 
                }
                
                if (publication.getFormType() == PublicationFormType.CONTRACT_NOTICE && publication.getIsIncluded()) {
                    if (minNoticeDate == null || minNoticeDate.isAfter(
                            publication.getPublicationDate())) {
                        minNoticeDate = publication.getPublicationDate();
                    } 
                }
            }
        }
        
        if (minContractAwardDate != null) {
            return minContractAwardDate;
        } else if (minNoticeDate != null) {
            return minNoticeDate;
        } else {
            return minDate;
        }
    }

    /**
     * This methods converts price to national currencies and EUR.
     * @param price price to be converted
     * @param date defines date, fow whixh should be the exchange rate taken
     */
    private void convertPrice(final BasePrice price, final LocalDate date) {
       if (price == null) {
           return;
       }
       try {
           if (price.getNetAmount() != null) {
               if (price.getNetAmountEur() == null) {
                   price.setNetAmountEur(currencyService.convert(
                           price.getCurrency(), Currency.getInstance("EUR"), price.getNetAmount(), date));
               }
               
               price.setCurrencyNational(getNationalCurrency());
               price.setNetAmountNational(currencyService.convert(
                       price.getCurrency(), price.getCurrencyNational(), price.getNetAmount(), date));
           }
       } catch (UnconvertableException e) {
           logger.error("Unable to convert prices because of {}", e);
       }
    }

    /**
     * Returns national currency relevant for this source.
     * @return national currency
     */
    protected final Currency getNationalCurrency() {
        String propertyPrefix = StringUtils.substring(getName(), 0, StringUtils.ordinalIndexOf(getName(), ".", 4));
        String currency = config.getParam(propertyPrefix + ".currency");

        if (currency == null) {
            logger.warn("Unable to get currency for '{}'", getName());
            return null;
        }

        return Currency.getInstance(currency);
    }
}
