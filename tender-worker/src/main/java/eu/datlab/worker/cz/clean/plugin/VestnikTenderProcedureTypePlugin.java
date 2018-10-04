package eu.datlab.worker.cz.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.CodeTablePlugin;
import eu.dl.worker.clean.utils.CodeTableUtils;
import org.apache.commons.lang.BooleanUtils;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Plugins used to clean procedure type field.
 *
 * @author Tomas Mrazek
 */
public final class VestnikTenderProcedureTypePlugin extends CodeTablePlugin<ParsedTender, CleanTender> {
    private final List<String> accelerated;

    /**
     * Tender procedure type cleaner with mapping. In case that {@code accelerated} list contains cleaned value sets
     * cleanTender.isAccelerated to TRUE.
     *
     * @param mapping
     *         mapping of the values
     * @param accelerated
     *         list of parsed procedure types that are accelerated
     */
    public VestnikTenderProcedureTypePlugin(final Map<Enum, List<String>> mapping, final List<String> accelerated) {
        super(mapping);
        this.accelerated = accelerated;
    }

    /**
     * Tender procedure type cleaner with mapping.
     *
     * @param mapping
     *         mapping of the values
     */
    public VestnikTenderProcedureTypePlugin(final Map<Enum, List<String>> mapping) {
        super(mapping);
        this.accelerated = null;
    }

    /**
     * Cleans tender procedure type field.
     *
     * @param parsedTender
     *         tender with source data
     * @param cleanTender
     *         tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        final String parsedProcedureType = parsedTender.getNationalProcedureType();
        cleanTender.setNationalProcedureType(parsedProcedureType);

        final TenderProcedureType procedureType;

        // checks for minitender and dps purchase (such tenders have parent publication and
        // isFrameworkAgreement/isDps set to true)
        final boolean hasParentPublication = hasParentPublication(parsedTender);
        if (hasParentPublication && isFrameworkAgreement(parsedTender)) {
            procedureType = TenderProcedureType.MINITENDER;
        } else if (hasParentPublication && isDps(parsedTender)) {
            procedureType = TenderProcedureType.DPS_PURCHASE;
        } else {
            // otherwise try to map the source value
            procedureType = (TenderProcedureType) CodeTableUtils.mapValue(parsedProcedureType, mapping);
        }
        cleanTender.setProcedureType(procedureType);

        if (parsedProcedureType != null && accelerated != null) {
            for (String value : accelerated) {
                if (parsedProcedureType.equalsIgnoreCase(value)) {
                    cleanTender.setIsAcceleratedProcedure(Boolean.TRUE);
                    break;
                }
            }

            logger.debug("Cleaned procedureType in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                    procedureType);
        }

        logger.debug("ProcedureType in parsed tender {} was null", parsedTender.getId());

        return cleanTender;
    }

    /**
     * Checks whether the tender has parent publication that is not included.
     *
     * @param parsedTender
     *         parsed tender
     *
     * @return true if the tender has parent publication that is not included
     */
    private static boolean hasParentPublication(final ParsedTender parsedTender) {
        return parsedTender.getPublications()
                .stream()
                .filter(Objects::nonNull)
                .anyMatch(publication -> isParentPublication(publication));
    }

    /**
     * Checks whether the publication is parent and not included.
     *
     * @param publication
     *         parsed publication
     *
     * @return true if the publication is parent publication and not included
     */
    private static boolean isParentPublication(final ParsedPublication publication) {
        return publication != null && BooleanUtils.toBoolean(
                publication.getIsParentTender()) && !BooleanUtils.toBoolean(publication.getIsIncluded());
    }

    /**
     * Checks whether the tender was parsed as framework agreement/minitender.
     *
     * @param parsedTender
     *         parsed tender
     *
     * @return true if the tender was parsed as framework agreement/minitender
     */
    private static boolean isFrameworkAgreement(final ParsedTender parsedTender) {
        return BooleanUtils.toBoolean(parsedTender.getIsFrameworkAgreement());
    }

    /**
     * Checks whether the tender was parsed as DPS/DPS purchase.
     *
     * @param parsedTender
     *         parsed tender
     *
     * @return true if the tender was parsed as DPS/DPS purchase
     */
    private static boolean isDps(final ParsedTender parsedTender) {
        return BooleanUtils.toBoolean(parsedTender.getIsDps());
    }
}
