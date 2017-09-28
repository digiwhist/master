package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.master.BaseMasterTenderLot;
import eu.dl.dataaccess.dto.matched.BaseMatchedTenderLot;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.worker.utils.BasePlugin;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Plugin class which does set union of CPVs. Only one can be main:
 *  - the more specific one (marked as main)
 *  - if more same specific then the last one
 *
 * @param <T>
 *         item type to be mastered
 * @param <V>
 *         master item type
 * @param <U>
 *         root items - context
 */
public final class CpvPlugin<T extends BaseMatchedTenderLot & MasterablePart, V extends BaseMasterTenderLot, U>
        extends BasePlugin implements MasterPlugin<T, V, U> {

    @Override
    public V master(final List<T> items, final V finalItem, final List<U> context) {
        CPV masteredMainCpv = null;
        LocalDate masteredMainCpvPublicationDate = null;        
        List<CPV> masteredCpvs = (finalItem.getCpvs() == null ? new ArrayList<>() : finalItem.getCpvs());

        for (T item : items) {
            List<CPV> cpvs = item.getCpvs();
            if (cpvs == null || cpvs.isEmpty()) {
                continue;
            }

            LocalDate cpvPublicationDate = item.getPublicationDate();
            
            for (CPV cpv : cpvs) {
                if (cpv == null) {
                    continue;
                }
                
                if (masteredCpvs.stream()
                        .noneMatch(c -> (cpv.getCode() != null && cpv.getCode().equals(c.getCode())))) {
                    if (cpv.getIsMain() != null && cpv.getIsMain()) {
                        CPV best = (masteredMainCpv == null ? cpv : getBestMainCpv(cpv, masteredMainCpv,
                            cpvPublicationDate, masteredMainCpvPublicationDate));

                        if (best != null && best.equals(cpv)) {
                            masteredMainCpv = cpv;
                            masteredMainCpvPublicationDate = cpvPublicationDate;
                        }
                    }

                    masteredCpvs.add(cpv.setIsMain(false));
                }
            }
        }

        //set main CPV
        if (masteredMainCpv != null) {
            for (CPV cpv : masteredCpvs) {
                if (cpv.getCode() != null && cpv.getCode().equals(masteredMainCpv.getCode())) {
                    cpv.setIsMain(true);
                    break;
                }
            }
        }

        finalItem.setCpvs(masteredCpvs.isEmpty() ? null : masteredCpvs);
        
        return finalItem;
    }

    /**
     * Compares two main CPVs, decides which should be the master main CPV. Decision:
     *  - the more specific one (marked as main)
     *  - if more same specific then the last one
     *
     * @param cpv1
     *      first CPV
     * @param cpv2
     *      second CPV
     * @param cpv1PublicationDate
     *         publication date of first CPV
     * @param cpv2PublicationDate
     *         publication date of second CPV
     * @return the best one from {@code cpv1} and {@code cpv2}
     */
    private CPV getBestMainCpv(final CPV cpv1, final CPV cpv2,
        final LocalDate cpv1PublicationDate, final LocalDate cpv2PublicationDate) {
        
        if (cpv1.getCode() != null && cpv2.getCode() != null) {
            int cpv1FirstZeroIndex = cpv1.getCode().indexOf('0');
            int cpv2FirstZeroIndex = cpv2.getCode().indexOf('0');

            if (cpv1FirstZeroIndex < cpv2FirstZeroIndex) {
                // second CPV is more specific
                return cpv2;
            } else if (cpv1FirstZeroIndex > cpv2FirstZeroIndex) {
                // first CPV is more specific
                return cpv1;
            } else {
                // CPVs are same specific, so we want the one which is last published
                if ((cpv2PublicationDate == null && cpv1PublicationDate == null)
                    || (cpv2PublicationDate != null && cpv1PublicationDate == null)) {
                    return cpv2;
                } else if (cpv2PublicationDate == null && cpv1PublicationDate != null) {
                    return cpv1;
                } else if (cpv1PublicationDate.isBefore(cpv2PublicationDate)) {
                    return cpv2;
                } else if (cpv2PublicationDate.isBefore(cpv1PublicationDate)) {
                    return cpv1;
                } else {
                    return cpv2;
                }
            }
        } else if (cpv1.getCode() != null) {
            return cpv1;
        } else if (cpv2.getCode() != null) {
            return cpv2;
        }

        return null;
    }
}
