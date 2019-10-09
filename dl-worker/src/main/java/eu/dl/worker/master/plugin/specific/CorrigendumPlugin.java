package eu.dl.worker.master.plugin.specific;

import eu.dl.dataaccess.dto.codetables.CorrectionType;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.master.plugin.specific.corrigendum.executors.BaseCorrectionExecutor;
import eu.dl.worker.master.plugin.specific.corrigendum.executors.BaseDateCorrectionExecutor;
import eu.dl.worker.master.plugin.specific.corrigendum.executors.BaseListCPVCorrectionExecutor;
import eu.dl.worker.utils.BasePlugin;

import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
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
            BaseCorrectionExecutor plugin = getPlugin(c);
            if (plugin != null) {
                plugin.execute(c, finalItem);
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
     * @param c correction
     * @return appropriate plugin for the execution of a correction
     */
    private BaseCorrectionExecutor getPlugin(final Corrigendum c) {
        if (c.getSectionNumber() == null) {
            return null;
        }

        String section = c.getSectionNumber().replaceAll("^([IV]+(\\.\\d)+).*", "$1");

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

                    @Override
                    protected boolean isEqualToThePreviousValue(final MasterTender t, final LocalDateTime origin) {
                        for (Corrigendum c : t.getCorrections()) {
                            if (c.getIsIncluded() != null && c.getIsIncluded() && c.getType().equals(CorrectionType.BID_DEADLINE)) {
                                if (isEqual(c.getOriginalDate(), origin) || isEqual(c.getReplacementDate(), origin)) {
                                    return true;
                                }
                            }
                        }
                        return false;
                    }
                };

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

                    @Override
                    protected boolean isEqualToThePreviousValue(final MasterTender t, final List<CPV> origin) {
                        for (Corrigendum c : t.getCorrections()) {
                            if (c.getIsIncluded() != null && c.getIsIncluded() && c.getType().equals(CorrectionType.CPVS)) {
                                if (isEqual(c.getOriginalCpvs(), origin) || isEqual(c.getReplacementCpvs(), origin)) {
                                    return true;
                                }
                            }
                        }
                        return false;
                    }
                };

            default:
                return null;
        }
    }
}