package eu.digiwhist.worker.lv.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import eu.dl.worker.clean.utils.CodeTableUtils;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Plugins used to clean procedure type field.
 *
 * @author Tomas Mrazek
 */
public final class IUBTenderFtpProcedureTypePlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    /**
     * Tender regulation type.
     */
    private enum RegulationType {
        /**
         * PIL.
         */
        PIL,
        /**
         * SPSIL.
         */
        SPSIL,
        /**
         * ADJIL.
         */
        ADJIL,
        /**
         * PPP.
         */
        PPP,
        /**
         * MK 65.
         */
        MK65,
        /**
         * MK 299.
         */
        MK299
    }

    /**
     * Cleans tender procedure type field.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        logger.debug("Cleaning of procedureType in parsed tender {} starts", parsedTender.getId());

        List<ParsedPublication> publications = parsedTender.getPublications();
        String sourceFormType = null;
        // attempt to get non-null sourceFormType from first included publication
        if (publications != null) {
            sourceFormType = publications.stream()
                .filter(p -> Boolean.parseBoolean(p.getIsIncluded()) && p.getSourceFormType() != null)
                .map(p -> p.getSourceFormType())
                .findFirst().orElse(null);
        }
        
        RegulationType regulation = getRegulationForSourceFormType(sourceFormType);
        logger.debug("Regulation type determined as {}", regulation);
        Map<Enum, List<String>> mapping = getMappingForSourceFormType(regulation);

        final TenderProcedureType procedureType =
            (TenderProcedureType) CodeTableUtils.mapValue(parsedTender.getNationalProcedureType(), mapping);
        cleanTender.setProcedureType(procedureType);

        logger.debug("Cleaned procedureType in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            procedureType);

        return cleanTender;
    }

    /**
     * @param regulation
     *      regulation type
     * @return mapping for the given regulation, otherwise mapping for PIL regulation if the passed regulation is
     * unsupported or is null
     */
    private Map<Enum, List<String>> getMappingForSourceFormType(final RegulationType regulation) {
        switch (regulation) {
            case ADJIL:
                return getADJILMapping();
            case PPP:
                return getPPPMapping();
            case SPSIL:
                return getSPSILMapping();
            case PIL:
            default:
                return getPILMapping();
        }
    }

    /**
     * Determines regulation on the base of the given source form type.
     *
     * @param sourceFormType
     *      source form type
     * @return regulation, otherwise PIL if the passed source form type is unsupported or is null
     */
    private RegulationType getRegulationForSourceFormType(final String sourceFormType) {
        if (sourceFormType != null) {
            if (sourceFormType.matches("^sps_.+$") && sourceFormType.matches("^.+_sps$")) {
                return RegulationType.SPSIL;
            } else if (sourceFormType.matches("^.+_81$")) {
                return RegulationType.ADJIL;
            } else if (sourceFormType.matches("^notice_concession(_.+)?$")) {
                return RegulationType.PPP;
            } else if (sourceFormType.matches("^notice_call_for_offer(_.+)?$")) {
                return RegulationType.MK65;
            } else if (sourceFormType.matches("^notice_299_.+$")) {
                return RegulationType.MK299;
            } else if (sourceFormType.matches("^notice_.+$")) {
                return RegulationType.PIL;
            }
        }

        return RegulationType.PIL;
    }

    /**
     * @return TenderProcedureType mapping for PIL regulation
     */
    private Map<Enum, List<String>> getPILMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("1"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("2", "3"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("4", "5", "7"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("8"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("9"));
        mapping.put(TenderProcedureType.DESIGN_CONTEST, Arrays.asList("100"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList("6"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("10"));

        return mapping;
    }

    /**
     * @return TenderProcedureType mapping for SPSIL regulation
     */
    private Map<Enum, List<String>> getSPSILMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("1"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("2"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("7"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("8"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("11"));

        return mapping;
    }

    /**
     * @return TenderProcedureType mapping for ADJIL regulation
     */
    private Map<Enum, List<String>> getADJILMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("2", "3"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("4", "5", "7"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("8"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("9"));

        return mapping;
    }

    /**
     * @return TenderProcedureType mapping for PPP regulation
     */
    private Map<Enum, List<String>> getPPPMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("9"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("10", "11"));

        return mapping;
    }
  }
