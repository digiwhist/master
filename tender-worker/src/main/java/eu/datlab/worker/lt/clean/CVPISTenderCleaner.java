package eu.datlab.worker.lt.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.SelectionMethodPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import org.apache.commons.lang3.StringUtils;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Created by michalriha on 05/06/2017.
 */
public class CVPISTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("lt");
    private static final List<NumberFormat> NUMBER_FORMATS = new ArrayList<>();
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator('.');
        NUMBER_FORMATS.add(new DecimalFormat("#,##0.0##", formatSymbols));
    }
    private static final List<DateTimeFormatter> DATE_FORMATTER = Arrays.asList(
            DateTimeFormatter.ofPattern("uuuu-MM-dd"),
            DateTimeFormatter.ofPattern("uuuu.MM.dd"));
    private static final List<DateTimeFormatter> DATETIME_FORMATTER = Arrays.asList(
            DateTimeFormatter.ofPattern("uuu-MM-dd hh:mm"),
            DateTimeFormatter.ofPattern("uuu.MM.dd hh:mm"),
            DateTimeFormatter.ofPattern("'Data: 'uuuu-MM-dd' Laikas: 'HH:mm"));

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        final Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("countryMapping", countryMapping());

        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMATS))
                .registerPlugin("procedureType",
                        new TenderProcedureTypePlugin(procedureTypeMapping(), Arrays.asList("5. Pagreitintų derybų")))
                .registerPlugin("date", new DatePlugin(DATETIME_FORMATTER))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
                .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), bodyActivityMapping(), countryMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMATS, DATE_FORMATTER, lotMappings))
                .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMATS, DATE_FORMATTER, null))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMATS))
                .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMATS))
                .registerPlugin("corrections", new CorrigendumPlugin(NUMBER_FORMATS, DATE_FORMATTER))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()))
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMATS))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMATS, DATE_FORMATTER, formTypeMapping()))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()));
    }

    /**
     * @return form supply type mapping
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Pirkimas", "Nuoma",
                "Lizingas", "Pirkimas išsimokėtinai", "Mišrus"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Atlikti darbus", "Atlikti ir suprojektuoti darbus",
                "Bet kokiomis priemonėmis atlikti darbus, atitinkankčius perkančiosios organizacijos " +
                        "nustatytus reikalavimus"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("SERVICES"));
        // services should be default value

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("SKELBIMAS APIE SUPAPRASTINTÄ„ PIRKIMÄ„",
                "SKELBIMAS APIE SUPAPRASTINTÄ„ MAĹ˝OS VERTÄ–S PIRKIMÄ„", "SKELBIMAS APIE PIRKIMÄ„", "SKELBIMAS APIE PIRKIMďż˝",
                "SKELBIMAS APIE PIRKIMÄ„ - KOMUNALINÄ–S PASLAUGOS", "SKELBIMAS APIE PIRKIMÄ„ GYNYBOS IR SAUGUMO SRITYJE",
                "SKELBIMAS APIE SUPAPRASTINTĄ MAŽOS VERTĖS PIRKIMĄ", "SKELBIMAS APIE SUPAPRASTINTĄ PIRKIMĄ"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("SKELBIMAS DÄ–L PAPILDOMOS INFORMACIJOS ARBA PATAISOS",
                "SKELBIMAS DĖL PAPILDOMOS INFORMACIJOS ARBA PATAISOS",
                "SKELBIMAS DÄ–L PAPILDOMOS INFORMACIJOS ARBA PATAISOS", // ?
                "SKELBIMAS, SUSIJĘS SU PAPILDOMA INFORMACIJA, INFORMACIJA APIE NEUŽBAIGTĄ PROCEDŪRĄ ARBA PATAISA")); // ?
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Skelbimas apie sutarties sudarymÄ…",
                "SKELBIMAS APIE SUTARTIES SUDARYMÄ„- KOMUNALINÄ–S PASLAUGOS", "At-8"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("ŠANKSTINIS INFORMACINIS SKELBIMAS",
                "IĹ ANKSTINIS INFORMACINIS SKELBIMAS", "IŠANKSTINIS INFORMACINIS SKELBIMAS"));

        return mapping;
    }

    /**
     * @return selection Method mapping for cleaning process
     */
    private Map<Enum, List<String>> selectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(SelectionMethod.MEAT, Arrays.asList("Ekonomiškai naudingiausias pasiūlymas"));
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("Mažiausia kaina"));

        return mapping;
    }

    /**
     * @return body type mapping
     */
    private static Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Ministerija ar kuri nors kita nacionalinės ar federaci" +
                "nės valdžios institucija, įskaitant jų regioninius ar vietos padalinius"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("Nacionalinė ar federacinė tarnyba ar biuras"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("Regiono ar vietos tarnyba ar biuras"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("Regiono ar vietos valdžios institucija"));
        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Akcinė bendrovė",
                "Akcinė bendrovė, kuri yra perkančioji organizacija pagal LR Viešųjų pirkimų įstatymą"));
        mapping.put(BuyerType.OTHER, Arrays.asList("Kita"));

        return mapping;
    }

    /**
     * @return body activities mapping
     */
    private static Map<Enum, List<String>> bodyActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("Bendros viešosios paslaugos"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("Aplinka"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS,
                Arrays.asList("Ekonomikos ir finansiniai reikalai"));
        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("Gynyba"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION,
                Arrays.asList("Laisvalaikis, kultūra ir religija"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("Socialinė apsauga"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Sveikata"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("Švietimas"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("Viešoji tvarka ir visuomenės apsauga"));
        mapping.put(BuyerActivityType.HOUSING_AND_COMMUNITY_AMENITIES,
                Arrays.asList("Būsto ir komunaliniai patogumai"));
        mapping.put(BuyerActivityType.OTHER, Arrays.asList("Kita"));

        return mapping;
    }

    /**
     * @return precedure type mapping
     */
    private Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Atvira", "4. Pagreitinta ribota", "1. Atvira"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("2. Ribota"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("16. Konkurencinis dialogas"));
        mapping.put(TenderProcedureType.NEGOTIATED,
                Arrays.asList("5. Pagreitintų derybų", "3. Derybų"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION,
                Arrays.asList("14. Derybų procedūra kai skelbimas dalyvauti konkurse nėra skelbiamas",
                        "12. Derybų procedūra paskelbiant skelbimą dalyvauti konkurse"));

        return mapping;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        // selection methods
        if (parsedItem.getSelectionMethod() != null) {
            if (parsedItem.getSelectionMethod().contains("Mažiausia kaina") || parsedItem.getSelectionMethod()
                    .contains("Ma�iausia kaina")) {
                parsedItem.setSelectionMethod("Mažiausia kaina");
            } else if (parsedItem.getSelectionMethod().contains("Ekonomiškai naudingiausias pasiūlymas")) {
                parsedItem.setSelectionMethod("Ekonomiškai naudingiausias pasiūlymas");
            } else {
                parsedItem.setSelectionMethod(null);
            }
        }

        // buyerAssignedId field contains unneeded text before id, id is always only digits here
        if (parsedItem.getBuyerAssignedId() != null) {
            parsedItem.setBuyerAssignedId(parsedItem.getBuyerAssignedId().replaceAll("\\D", ""));
        }

        // buyername, bodyid, phone
        if (parsedItem.getBuyers() != null && !parsedItem.getBuyers().isEmpty()) {
            parsedItem.getBuyers().set(0, cleanBody(parsedItem.getBuyers().get(0)));
        }

        // bidder
        if (parsedItem.getLots() != null) {
            for (final ParsedTenderLot lot : parsedItem.getLots()) {
                if (lot.getBids() != null) {
                    for (final ParsedBid bid : lot.getBids()) {
                        if (bid.getPrice() != null) {
                            bid.setPrice(cleanPrice(bid.getPrice()));
                        }

                        if (bid.getBidders() != null) {
                            for (int i = 0; bid.getBidders().size() > i; i++) {
                                bid.getBidders().set(i, cleanBody(bid.getBidders().get(i)));
                            }
                        }
                    }
                }
            }
        }

        // publication sourceId
        if (parsedItem.getPublications() != null) {
            for (final ParsedPublication publication : parsedItem.getPublications()) {
                if (publication.getSourceId() != null) {
                    final String[] sourceIdAndDate = publication.getSourceId().split(",");

                    if (sourceIdAndDate.length > 1) {
                        publication.setSourceId(sourceIdAndDate[0]);
                        publication.setPublicationDate(sourceIdAndDate[1].replaceAll("\\(.*", ""));
                    }
                }

                if (publication.getDispatchDate() != null) {
                    publication.setDispatchDate(publication.getDispatchDate().replaceAll("\\(.*", ""));
                }
            }
        }

        // finalprice
        if (parsedItem.getFinalPrice() != null) {
            parsedItem.setFinalPrice(cleanPrice(parsedItem.getFinalPrice()));
        }

        // cpvs
        if (parsedItem.getCpvs() != null && !parsedItem.getCpvs().isEmpty() && parsedItem.getCpvs().get(0).getCode()
                != null && parsedItem.getCpvs().get(0).getCode().contains(",")) {
            final String[] cpvs = parsedItem.getCpvs().get(0).getCode().split(",");

            final List<ParsedCPV> parsedCPVs = new ArrayList<>();
            boolean isMain = true;
            for (String cpv : cpvs) {
                parsedCPVs.add(parsedItem.getCpvs().get(0).setCode(cpv).setIsMain(Boolean.toString(isMain)));
                isMain = false;
            }

            parsedItem.setCpvs(parsedCPVs);
        }

        if (parsedItem.getIsOnBehalfOf() != null) {
            parsedItem.setIsOnBehalfOf(parseToBoolean(parsedItem.getIsOnBehalfOf()));
        }

        if (parsedItem.getIsDps() != null) {
            parsedItem.setIsDps(parseToBoolean(parsedItem.getIsDps()));
        }

        if (parsedItem.getIsCoveredByGpa() != null) {
            parsedItem.setIsCoveredByGpa(parseToBoolean(parsedItem.getIsCoveredByGpa()));
        }

        if (parsedItem.getIsElectronicAuction() != null) {
            parsedItem.setIsElectronicAuction(parseToBoolean(parsedItem.getIsElectronicAuction()));
        }

        if (parsedItem.getIsFrameworkAgreement() != null) {
            parsedItem.setIsFrameworkAgreement(parseToBoolean(parsedItem.getIsFrameworkAgreement()));
        }

        return parsedItem;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender
            cleanItem) {
        return cleanItem;
    }

    /**
     * @return country mapping.
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.BY, Arrays.asList("Baltarusija"));
        mapping.put(CountryCode.BE, Arrays.asList("Belgija"));
        mapping.put(CountryCode.LT, Arrays.asList("Lietuva"));
        mapping.put(CountryCode.RU, Arrays.asList("Rusijos Federacija"));

        return mapping;
    }

    /**
     * Clean body.
     *
     * @param body to clean
     * @return ParsedBody
     */
    private static ParsedBody cleanBody(final ParsedBody body) {
        if (body.getName() != null) {
            final String[] cleanNameAndId = body.getName().split("\\(");

            if (cleanNameAndId.length > 1) {
                body.setName(cleanNameAndId[0]);
                body.getBodyIds().get(0).setId(cleanNameAndId[1].replace(")", ""));
            }
        }

        if (body.getPhone() != null) {
            final String[] arrayWithPhone = body.getPhone().split(",");

            for (String possiblePhone : arrayWithPhone) {
                if (possiblePhone.contains("tel")) {
                    body.setPhone(possiblePhone.replaceAll("[a-z\\.]", ""));
                }
            }
        }

        if (body.getBuyerType() != null && body.getMainActivities() != null && !body.getMainActivities().isEmpty()
                && body.getBuyerType().contains(".")) {
            final String[] buyerTypeAndMainActivity = body.getBuyerType().split(".");

            if (buyerTypeAndMainActivity.length > 1) {
                body.setBuyerType(buyerTypeAndMainActivity[0]);
                body.getMainActivities().set(0, buyerTypeAndMainActivity[1]);
            }
        }

        return body;
    }

    /**
     * Clean price.
     *
     * @param parsedPrice price to clean
     * @return ParsedPrice
     */
    private static ParsedPrice cleanPrice(final ParsedPrice parsedPrice) {
        if (parsedPrice.getNetAmount() != null) {
            parsedPrice.setNetAmount(parsedPrice.getNetAmount()
                    .replaceAll("[a-žA-Ž:]", ""));
        }

        if (parsedPrice.getAmountWithVat() != null) {
            parsedPrice.setAmountWithVat(parsedPrice.getAmountWithVat()
                    .replaceAll("[a-žA-Ž:]", ""));
        }

        if (parsedPrice.getCurrency() != null) {
            parsedPrice.setCurrency(parsedPrice.getCurrency()
                    .replaceAll("[a-žA-Ž:]", "").trim().replaceAll(" .*", ""));
        }

        if (parsedPrice.getVat() != null) {
            parsedPrice.setVat(parsedPrice.getVat()
                    .replaceAll("[a-žA-Ž:]", ""));
        }

        return parsedPrice;
    }

    /**
     * Check weather string contains text parsable to boolean.
     *
     * @param string string to clean
     * @return String or null
     */
    private static String parseToBoolean(final String string) {
        if (string == null) {
            return null;
        } else if (StringUtils.containsIgnoreCase("Taip", string)
                || StringUtils.containsIgnoreCase("True", string)) {
            return Boolean.TRUE.toString();
        } else if (StringUtils.containsIgnoreCase("No", string)
                || StringUtils.containsIgnoreCase("False", string)) {
            return Boolean.FALSE.toString();
        } else {
            return string;
        }
    }
}
