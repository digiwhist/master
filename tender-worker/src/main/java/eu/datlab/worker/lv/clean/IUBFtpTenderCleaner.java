package eu.datlab.worker.lv.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderSize;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.SelectionMethodPlugin;
import eu.dl.worker.clean.plugin.TenderSizePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import eu.dl.worker.clean.utils.CodeTableUtils;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;

/**
 * Cleans tenders from ftp://open.iub.gov.lv (Latvia).
 *
 * @author Tomas Mrazek
 */
public final class IUBFtpTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.1";

    private static final Locale LOCALE = new Locale("en");

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(',');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    /**
     * This DateTimeFormatter parses following datetime strings.
     * - dd/MM/yyyy
     * - dd/MM/yyyy H:m
     * - dd/MM/yyyy H-m
     * - dd/MM/yyyy H.m
     */
    private static final List<DateTimeFormatter> DATETIME_FORMATTER = Arrays.asList(
        new DateTimeFormatterBuilder().append(DATE_FORMATTER).appendPattern("[ H:m]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .toFormatter(),
        new DateTimeFormatterBuilder().append(DATE_FORMATTER).appendPattern("[ H-m]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .toFormatter(),
        new DateTimeFormatterBuilder().append(DATE_FORMATTER).appendPattern("[ H.m]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .toFormatter()
    );

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("integer", new IntegerPlugin(NUMBER_FORMAT))
            .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))            
            .registerPlugin("date", new DatePlugin(DATE_FORMATTER))
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
            .registerPlugin("bodies", new BodyPlugin(null, null, countryMapping()))
            .registerPlugin("publications",
                new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTER, formTypeMapping()))            
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTER, new HashMap<>()))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT))
            .registerPlugin("corrections", new CorrigendumPlugin(NUMBER_FORMAT, DATE_FORMATTER))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
            .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()))
            .registerPlugin("tenderSize", new TenderSizePlugin(tenderSizeMapping()))
            .registerPlugin("procedureType", new IUBTenderFtpProcedureTypePlugin());
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
        final CleanTender cleanTender) {

        // mapping of nuts ids in addressOfImplementation (other addresses doesn't include nuts).
        Address address = cleanTender.getAddressOfImplementation();
        if (address != null && address.getNuts() != null) {
            ListIterator litr = address.getNuts().listIterator();
            while (litr.hasNext()) {
               String id = (String) litr.next();
               if (id != null) {
                    String nuts = mapNuts(id);
                    litr.set(nuts);

                    logger.debug("Cleaned addressOfImplementation.nuts in parsed tender {}, clean value \"{}\"",
                        parsedTender.getId(), nuts);
               }
            }
        }

        return cleanTender;
    }

    /**
     * Maps nuts id to the nuts code.
     *
     * @param nutsId
     *      mapped nuts id
     * @return nuts code for the known id or {@code nutsId} for unknown id and or null if null is passed
     */
    private String mapNuts(final String nutsId) {
        if (nutsId == null) {
            return null;
        }

        switch (nutsId) {
            case "1":
                return "LV00";
            case "3": case "5": case "6": case "7": case "8": case "9":
                return "LV00" + nutsId;
            case "10":
                return "LVZ";
            case "11":
                return "LVZZ";
            case "12":
                return "LVZZZ";
            default:
                return nutsId;
        }
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> supplyTypeMapping() {
        return CodeTableUtils.enumToMapping(TenderSupplyType.class);
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {        
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("1", "10", "12", "notice_iip",
            "sps_periodic", "notice_iip_81"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("4", "5", "8", "notice_contract",
            "notice_design", "notice_planned_contract", "notice_contract_sps", "sps_design", "notice_contract_81",
            "notice_concession", "notice_call_for_offer"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("6", "9", "13", "notice_concluded_contract",
            "notice_contract_rights", "notice_exante", "notice_design_results", "notice_simple_contract",
            "notice_contract_rights_sps", "sps_design_results", "sps_simple_contract", "sps_notice_exante",
            "notice_contract_rights_81", "notice_exante_81", "notice_concession_results", "notice_concession_building",
            "notice_concession_contract", "notice_concession_exante", "notice_call_for_offer_results"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("2", "7", "11", "notice_client_profile",
            "sps_client_profile", "sps_qualification", "notice_client_profile_81", "notice_contract_sub_81",
            "notice_299_contract", "notice_299_changes"));
        mapping.put(PublicationFormType.CONTRACT_AMENDMENT, Arrays.asList("notice_changes", "sps_notice_changes",
            "notice_changes_81", "notice_call_for_offer_changes"));

        return mapping;
    }

    /**
     * @return selection method mapping
     */
    private Map<Enum, List<String>> selectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("3"));
        mapping.put(SelectionMethod.MEAT, Arrays.asList("1", "2"));
        
        return mapping;
    }

    /**
     * @return tender size mapping
     */
    private Map<Enum, List<String>> tenderSizeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        
        mapping.put(TenderSize.ABOVE_THE_THRESHOLD, Arrays.asList("3", TenderSize.ABOVE_THE_THRESHOLD.name()));
        mapping.put(TenderSize.BELOW_THE_THRESHOLD, Arrays.asList("2"));

        return mapping;
    }

    /**
     * @return country type mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(CountryCode.LV, Arrays.asList("1"));

        return mapping;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
