package eu.dl.worker.master.plugin.specific;

import eu.dl.dataaccess.dto.codetables.CorrectionType;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.master.plugin.specific.corrigendum.executors.BaseDateCorrectionExecutor;
import eu.dl.worker.master.plugin.specific.corrigendum.executors.BaseListCPVCorrectionExecutor;
import eu.dl.worker.master.plugin.specific.corrigendum.executors.BasePriceCorrectionExecutor;
import eu.dl.worker.master.plugin.specific.corrigendum.executors.CorrectionExecutor;
import eu.dl.worker.utils.BasePlugin;
import org.apache.commons.lang3.tuple.Pair;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * This plugin applies corrections.
 */
public final class CorrigendumPlugin extends BasePlugin implements MasterPlugin<MatchedTender, MasterTender, MatchedTender> {
    @Override
    public MasterTender master(final List<MatchedTender> items, final MasterTender finalItem, final List<MatchedTender> context) {
        if (finalItem.getCorrections() == null) {
            return finalItem;
        }

        List<Corrigendum> corrections = finalItem.getCorrections().stream()
            .filter(c -> c.getPublicationDate() != null)
            .sorted(Comparator.comparing(Corrigendum::getPublicationDate))
            .collect(Collectors.toList());

        int order = 1;
        for (Corrigendum c : corrections) {
            List<CorrectionExecutor> plugins = getPlugins(c);
            if (!plugins.isEmpty()) {
                for (CorrectionExecutor p : plugins) {
                    p.execute(c, finalItem);
                }
                if (Boolean.TRUE.equals(c.getIsIncluded())) {
                    c.setProcessingOrder(order++);
                }
            } else {
                c.setIsIncluded(false);
            }
        }

        return finalItem;
    }

    /**
     * @param c
     *      correction
     * @return list of appropriate plugins for the execution of a correction
     */
    private List<CorrectionExecutor> getPlugins(final Corrigendum c) {
        if (c.getSectionNumber() == null) {
            return Collections.emptyList();
        }

        String section = c.getSectionNumber().replaceAll("^([IV]+(\\.\\d+)+).*", "$1");

        List<CorrectionExecutor> plugins = new ArrayList<>();

        plugins.add(getDateCorrectionExecutor(section));

        plugins.add(getCpvsCorrectionExecutor(section));

        plugins.addAll(getPriceCorrectionExecutor(section));

        return plugins.stream().filter(Objects::nonNull).collect(Collectors.toList());
    }

    /**
     * @param section
     *      section number
     * @return date correction executor or null
     */
    private CorrectionExecutor getDateCorrectionExecutor(final String section) {
        switch (section) {
            case "III.3.1":
            case "IV.2.2":
            case "IV.3.4":
                return new BaseDateCorrectionExecutor() {
                    @Override
                    protected LocalDateTime getTenderOrigin(final MasterTender t) {
                        return t.getBidDeadline();
                    }

                    @Override
                    protected void applyReplacement(final MasterTender t, final LocalDateTime replacement) {
                        if (replacement.toLocalTime().toString().equals("00:00")) {
                            t.setBidDeadline(replacement.withHour(t.getBidDeadline().getHour()).withMinute(t.getBidDeadline().getMinute()));
                        } else {
                            t.setBidDeadline(replacement);
                        }
                    }

                    @Override
                    protected CorrectionType getCorrectionType() {
                        return CorrectionType.BID_DEADLINE;
                    }
                };
            default:
                return null;
        }
    }

    /**
     * @param section
     *      section number
     * @return CPVs list correction executor or null
     */
    private CorrectionExecutor getCpvsCorrectionExecutor(final String section) {
        switch (section) {
            case "II.1.2":
            case "II.1.6":
                return new BaseListCPVCorrectionExecutor() {
                    @Override
                    protected List<CPV> getTenderOrigin(final MasterTender t) {
                        return t.getCpvs();
                    }

                    @Override
                    protected void applyReplacement(final MasterTender t, final List<CPV> replacement) {
                        t.setCpvs(replacement);
                    }

                    @Override
                    protected CorrectionType getCorrectionType() {
                        return CorrectionType.CPVS;
                    }
                };
            default:
                return null;
        }
    }

    /**
     * @param section
     *      section number
     * @return price correction executor or null
     */
    private List<CorrectionExecutor> getPriceCorrectionExecutor(final String section) {
        List<CorrectionExecutor> executors = new ArrayList<>();
        switch (section) {
            // final price(s)
            case "V.2.4":
            case "IV.3.4":
                executors.add(new BasePriceCorrectionExecutor<MasterTender>() {
                    @Override
                    protected CorrectionType getCorrectionType() {
                        return CorrectionType.FINAL_PRICE;
                    }

                    @Override
                    protected List<Pair<Price, MasterTender>> getTenderOriginPrices(final MasterTender t) {
                        return Arrays.asList(Pair.of(t.getFinalPrice(), t));
                    }
                    @Override
                    protected boolean applyReplacementOnNull(final MasterTender context, final Price replacement) {
                        context.setFinalPrice(new Price());
                        applyReplacement(context.getFinalPrice(), replacement);
                        return true;
                    }
                });
                executors.add(new BasePriceCorrectionExecutor<MasterBid>() {
                    @Override
                    protected CorrectionType getCorrectionType() {
                        return CorrectionType.BID_PRICE;
                    }
                    @Override
                    protected List<Pair<Price, MasterBid>> getTenderOriginPrices(final MasterTender t) {
                        List<Pair<Price, MasterBid>> prices = new ArrayList<>();

                        if (t.getLots() != null) {
                            t.getLots().stream()
                                .map(MasterTenderLot::getBids).filter(Objects::nonNull).flatMap(List::stream)
                                .forEach(b -> {
                                    prices.add(Pair.of(b.getPrice(), null));
                                });
                        }

                        return prices;
                    }
                    @Override
                    protected boolean applyReplacementOnNull(final MasterBid context, final Price replacement) {
                        return false;
                    }
                });

                break;
            // estimated price(s)
            case "II.2.1":
            case "II.1.5":
            case "II.2.6":
                executors.add(new BasePriceCorrectionExecutor<MasterTender>() {
                    @Override
                    protected CorrectionType getCorrectionType() {
                        return CorrectionType.ESTIMATED_PRICE;
                    }
                    @Override
                    protected List<Pair<Price, MasterTender>> getTenderOriginPrices(final MasterTender t) {
                        return Arrays.asList(Pair.of(t.getEstimatedPrice(), t));
                    }

                    @Override
                    protected boolean applyReplacementOnNull(final MasterTender context, final Price replacement) {
                        context.setEstimatedPrice(replacement);
                        return true;
                    }
                });
                executors.add(new BasePriceCorrectionExecutor<MasterTenderLot>() {
                    @Override
                    protected CorrectionType getCorrectionType() {
                        return CorrectionType.LOT_ESTIMATED_PRICE;
                    }
                    @Override
                    protected List<Pair<Price, MasterTenderLot>> getTenderOriginPrices(final MasterTender t) {
                        List<Pair<Price, MasterTenderLot>> prices = new ArrayList<>();

                        if (t.getLots() != null) {
                            t.getLots().forEach(l -> {
                                prices.add(Pair.of(l.getEstimatedPrice(), null));
                            });
                        }

                        return prices;
                    }
                    @Override
                    protected boolean applyReplacementOnNull(final MasterTenderLot context, final Price replacement) {
                        return false;
                    }
                });

                break;
            // all prices (unable to decide which price to fix, so try any price)
            case "II.1.7":
            case "II.2.2":
                executors.add(new BasePriceCorrectionExecutor<MasterTender>() {
                    @Override
                    protected CorrectionType getCorrectionType() {
                        return CorrectionType.PRICE;
                    }
                    @Override
                    protected List<Pair<Price, MasterTender>> getTenderOriginPrices(final MasterTender t) {
                        List<Pair<Price, MasterTender>> origins = new ArrayList<>();
                        origins.add(Pair.of(t.getFinalPrice(), null));
                        origins.add(Pair.of(t.getEstimatedPrice(), null));

                        if (t.getLots() != null) {
                            t.getLots().forEach(l -> {
                                origins.add(Pair.of(l.getEstimatedPrice(), null));
                                if (l.getBids() != null) {
                                    l.getBids().forEach(b -> origins.add(Pair.of(b.getPrice(), null)));
                                }
                            });
                        }

                        return origins;
                    }
                    @Override
                    protected boolean applyReplacementOnNull(final MasterTender t, final Price replacement) {
                        return false;
                    }
                });

                break;
            default:
                return Collections.emptyList();
        }

        return executors;
    }
}