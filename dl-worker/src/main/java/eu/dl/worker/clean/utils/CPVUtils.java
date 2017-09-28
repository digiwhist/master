package eu.dl.worker.clean.utils;

import eu.dl.worker.utils.ArrayUtils;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import java.util.List;


/**
 * This class provide method for CPV cleaning.
 *
 * @author Tomas Mrazek
 */
public final class CPVUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private CPVUtils() {

    }

    /**
     * Cleans the given CPV.
     *
     * @param parsedCpv
     *            parsed CPV
     * @return cleaned CPV
     */
    public static CPV cleanCpv(final ParsedCPV parsedCpv) {
        if (parsedCpv == null) {
            return null;
        }

        return new CPV()
            .setCode(StringUtils.cleanShortString(parsedCpv.getCode()))
            .setIsMain(StringUtils.cleanBoolean(parsedCpv.getIsMain()));
    }

    /**
     * Cleans the given list of parsedCpvs. Also ensures that each CleanCPV will have non-null property isMain.
     * First CPV is main if none of them not.
     *
     * @param parsedCpvs
     *            list of parsed CPVs
     * @return list of cleaned CPVs
     */
    public static List<CPV> cleanCpvs(final List<ParsedCPV> parsedCpvs) {
        if (parsedCpvs == null || parsedCpvs.isEmpty()) {
            return null;
        }

        return ArrayUtils.walk(parsedCpvs, CPVUtils::cleanCpv);
    }
}
