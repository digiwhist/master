package eu.datlab.worker.py.master;

import eu.datlab.worker.master.BaseDatlabTenderMaster;
import eu.dl.dataaccess.dto.generic.Amendment;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Tender master for Paraguay.
 */
public class DNCPTenderMaster extends BaseDatlabTenderMaster {
    private static final String VERSION = "1.0";

    private static final int DAYS_IN_MONTH = 30;

    private static final int DAYS_IN_YEAR = 365;

    private final Comparator<Amendment> dateComparator = (a1, a2) -> {
        LocalDate d1 = a1.getPublicationDate();
        LocalDate d2 = a2.getPublicationDate();

        if (d1 == null && d2 == null) {
            return 0;
        } else if (d1 == null) {
            return -1;
        } else if (d2 == null) {
            return 1;
        }

        return d1.compareTo(d2);
    };


    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
            .unRegisterPlugin("Lots")
            .registerPlugin("DNCPLots", new DNCPTenderLotPlugin());
    }

    @Override
    protected final List<MatchedTender> sourceSpecificPreprocessData(final List<MatchedTender> items) {
        return items;
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        if (item.getLots() != null) {
            item.getLots().stream()
                .filter(l -> l.getAmendments() != null)
                .forEach(l -> {
                    // winning bid price if exists or use price '0 PYG'
                    Price winningPrice = Optional.ofNullable(l.getBids()).orElse(Collections.emptyList()).stream()
                        .filter(b -> Boolean.TRUE.equals(b.getIsWinning()) && b.getPrice() != null).findFirst().map(MasterBid::getPrice)
                        .orElse(new Price().setNetAmount(BigDecimal.ZERO).setCurrency(getNationalCurrency()));

                    // origin values for price and completion date
                    Price originalPrice = new Price().setNetAmount(winningPrice.getNetAmount()).setCurrency(winningPrice.getCurrency());

                    LocalDate completionDate = l.getEstimatedCompletionDate();

                    int days = 0;

                    // sort amendments ascending by date
                    List<Amendment> sorted = l.getAmendments().stream().sorted(dateComparator).collect(Collectors.toList());
                    for (Amendment a : sorted) {
                        // set originalPrice and recalculate updatedPrice increment to the total value
                        a.setOriginalPrice(originalPrice);

                        if (a.getUpdatedPrice() != null) {
                            originalPrice = new Price()
                                    .setNetAmount(originalPrice.getNetAmount().add(a.getUpdatedPrice().getNetAmount()))
                                    .setCurrency(originalPrice.getCurrency());
                        }

                        a.setUpdatedPrice(originalPrice);

                        // re-convert prices after update
                        convertPrice(a.getOriginalPrice(), pickConversionDate(item));
                        convertPrice(a.getUpdatedPrice(), pickConversionDate(item));

                        // recalculate duration in days/months increment to the date
                        if (completionDate != null) {
                            if (a.getEstimatedDurationInDays() != null) {
                                completionDate = completionDate.plus(a.getEstimatedDurationInDays(), ChronoUnit.DAYS);
                                a.setEstimatedCompletionDate(completionDate);
                            }
                            if (a.getEstimatedDurationInMonths() != null) {
                                completionDate = completionDate.plus(a.getEstimatedDurationInMonths(), ChronoUnit.MONTHS);
                                a.setEstimatedCompletionDate(completionDate);
                            }
                            if (a.getEstimatedDurationInYears() != null) {
                                completionDate = completionDate.plus(a.getEstimatedDurationInYears(), ChronoUnit.YEARS);
                                a.setEstimatedCompletionDate(completionDate);
                            }

                            // amendment includes only estimatedCompletionDate, durations are set to NULL
                            a.setEstimatedDurationInDays(null).setEstimatedDurationInMonths(null);
                        } else {
                            if (a.getEstimatedDurationInDays() != null) {
                                days += a.getEstimatedDurationInDays();
                            }
                            if (a.getEstimatedDurationInMonths() != null) {
                                days += a.getEstimatedDurationInMonths() * DAYS_IN_MONTH;
                            }
                            if (a.getEstimatedDurationInYears() != null) {
                                days += a.getEstimatedDurationInYears() * DAYS_IN_YEAR;
                            }

                            // amendment includes only estimatedDurationInDays, estimatedCompletionDate is NULL and
                            // estimatedDurationInMonths is NULL (re-calculated to days number)
                            a.setEstimatedDurationInDays(days).setEstimatedDurationInMonths(null);
                        }
                    }
                });
        }

        return item;
    }
}
