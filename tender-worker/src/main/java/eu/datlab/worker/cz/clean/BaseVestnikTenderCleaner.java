package eu.datlab.worker.cz.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.datlab.worker.cz.clean.plugin.VestnikLotPlugin;
import eu.datlab.worker.cz.clean.plugin.VestnikSelectionMethodPlugin;
import eu.datlab.worker.cz.clean.plugin.VestnikTenderProcedureTypePlugin;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.NpwpReasonPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Cleaner for the cz - vestnik source.
 *
 * @author Kuba Krafka
 */
public abstract class BaseVestnikTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat numberFormat;

    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(new Locale("cs_CZ"));
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator(' ');
        numberFormat = new DecimalFormat("#,##0.###", formatSymbols);
    }

    @Override
    protected final void registerSpecificPlugins() {
        final Map<String, Map<Enum, List<String>>> lotMapping = new HashMap<>();
        lotMapping.put("selectionMethodMapping", getSelectionMethodMapping());

        pluginRegistry.registerPlugin("integerPlugin", new IntegerPlugin(numberFormat))
            .registerPlugin("supplyType", new TenderSupplyTypePlugin(getSupplyTypeMapping()))
            .registerPlugin("procedureType", new VestnikTenderProcedureTypePlugin(getProcedureTypeMapping()))
            .registerPlugin("date", new DatePlugin(getDateFormatters()))
            .registerPlugin("datetime", new DateTimePlugin(getDateTimeFormatters()))
            .registerPlugin("bodies", new BodyPlugin(getBodyTypeMapping(), getBodyActivityMapping()))
            .registerPlugin("publications",
                    new PublicationPlugin(numberFormat, getDateFormatters(), getFormTypeMapping()))
            //document type isn't provided -> mapping = null
            //status isn't provided -> mapping = null
            //price unit isn't provided -> mapping = null
            .registerPlugin("lots", new VestnikLotPlugin(numberFormat, getDateFormatters(), lotMapping))
            //document type isn't provided -> mapping = null
            .registerPlugin("documents", new DocumentPlugin(numberFormat, getDateFormatters(), null))
            .registerPlugin("npwpReasons", new NpwpReasonPlugin(getNpwpReasonMapping(), getNpwpReasonFreeTextMapping()))
            .registerPlugin("prices", new PricePlugin(numberFormat))
            .registerPlugin("fundings", new FundingsPlugin(numberFormat))
            .registerPlugin("corrections", new CorrigendumPlugin(numberFormat, getDateFormatters()))
            .registerPlugin("selectionMethod", new VestnikSelectionMethodPlugin(getSelectionMethodMapping()))
            .registerPlugin("awardCriteria", new AwardCriteriaPlugin(numberFormat));

        registerVestnikOrVvzSpecificPlugin();
    }

    /**
     * Method registers another plugins.
     */
    protected abstract void registerVestnikOrVvzSpecificPlugin();

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
            final CleanTender cleanTender) {
        // for some forms we override procedure type and always map it to DESIGN CONTEST
        for (int i = 0; i < cleanTender.getPublications().size(); ++i) {
            Publication publication = cleanTender.getPublications().get(i);
            if (publication.getIsIncluded() != null && publication.getIsIncluded()) {
                final String sourceFormType = cleanTender.getPublications().get(i).getSourceFormType();
                if (sourceFormType != null && getDesignContestSourceFormTypes().contains(sourceFormType)) {
                    cleanTender.setProcedureType(TenderProcedureType.DESIGN_CONTEST);
                }
                break;
            }
        }

        // set simplified below-the-threshold procedure type if it is described in additional tender info
        // TODO:
        final String additionalInfo = cleanTender.getAdditionalInfo();
        if (additionalInfo != null && additionalInfo.matches(".*zjed.*podli.*")) {
            cleanTender.setProcedureType(TenderProcedureType.APPROACHING_BIDDERS);
        }

        // there might be some specific post processing rules different for Vestnik and VVZ
        postProcessVestnikOrVvzSpecificRules(parsedTender, cleanTender);

        return cleanTender;
    }

    /**
     * @return body activities mapping
     */
    protected abstract Map<Enum, List<String>> getBodyActivityMapping();

    /**
     * @return body type mapping
     */
    private static Map<Enum, List<String>> getBodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY,
                Arrays.asList("BODY_PUBLIC", "státní podnik", "příspěvková organizace", "zdravotní pojišťovna",
                        "jiná organizace, s.p.", "Příspěvková organizace", "Zdravotní pojišťovna",
                        "veřejná výzkumná instituce", "Česká národní banka"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("NATIONAL_AGENCY"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("MINISTRY"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("REGIONAL_AGENCY"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("REGIONAL_AUTHORITY"));
        mapping.put(BuyerType.EUROPEAN_AGENCY, Arrays.asList("EU_INSTITUTION"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    protected abstract Map<Enum, List<String>> getFormTypeMapping();

    /**
     * @return selection method mapping
     */
    private Map<Enum, List<String>> getSelectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("Nejnižší nabídková cena", "AC_PRICE"));
        mapping.put(SelectionMethod.MEAT,
                Arrays.asList("hospodářsky nejvýhodnější nabídka z hlediska", "hospodářsky nejvýhodnější nabídka",
                        "AC_PROCUREMENT_DOC", "AC_QUALITY_AC_COST"));

        return mapping;
    }

    /**
     * @return list of formatters used to parse the date fields
     */
    protected abstract List<DateTimeFormatter> getDateFormatters();

    /**
     * @return formatter used to parse the datetime fields
     */
    protected abstract List<DateTimeFormatter> getDateTimeFormatters();

    /**
     * @return supply type mapping for cleaning process
     */
    protected abstract Map<Enum, List<String>> getSupplyTypeMapping();

    /**
     * @return procedure type mapping for cleaning process
     */
    protected abstract Map<Enum, List<String>> getProcedureTypeMapping();

    /**
     * @return form types for which we override procedure type and always map it to DESIGN CONTEST
     */
    protected abstract List<String> getDesignContestSourceFormTypes();

    /**
     * @return procedure type mapping for cleaning process
     */
    protected abstract Map<Enum, List<String>> getNpwpReasonMapping();

    /**
     * @return procedure type mapping for cleaning process
     */
    protected abstract Map<Enum, List<List<String>>> getNpwpReasonFreeTextMapping();

    /**
     * Specific rules for Vestnik or VVZ for post processing of clean item.
     *
     * @param parsedTender
     *         parsed tender to be post processed
     * @param cleanTender
     *         clean tender to be post processed
     *
     * @return post processed clean tender
     */
    protected abstract CleanTender postProcessVestnikOrVvzSpecificRules(ParsedTender parsedTender,
            CleanTender cleanTender);
}

