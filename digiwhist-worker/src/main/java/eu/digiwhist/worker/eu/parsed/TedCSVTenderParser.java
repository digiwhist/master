package eu.digiwhist.worker.eu.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.StringUtils;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * TED CSV tenders parser.
 *
 * @author Tomas Mrazek
 */
public class TedCSVTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1.0";

    private final Logger logger = LoggerFactory.getLogger(TedCSVTenderParser.class);

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        logger.info("Parsing CSV from {} starts.", raw.getSourceUrl());

        List<ParsedTender> parsedTenders = new ArrayList<>();

        List<String> csvHeader = (List<String>) raw.getMetaData().get("csvHeader");
        if (csvHeader == null || csvHeader.isEmpty()) {
            logger.error("Unable to parse CSV because of unknown header.");
            throw new UnrecoverableException("Unable to parse CSV because of unknown header.");
        }

        try {
            final CSVParser parser = CSVParser.parse(raw.getSourceData(), CSVFormat.DEFAULT
                .withTrim(true)
                .withNullString("")                
                .withIgnoreSurroundingSpaces(true)
                .withIgnoreHeaderCase(true)
                .withHeader(csvHeader.toArray(new String[0])));
            
            parser.getRecords().forEach(r -> {
                ParsedTender parsedTender = parseCommonFormData(r, raw.getSourceUrl().toString());
                String type = r.get("id_type");
                if (type == null) {
                    logger.error("Unable to parse because column id_type is not set.");
                    throw new UnrecoverableException("Column id_type is not set.");
                }

                switch (type) {
                    case "2": case "4": case "5":
                        parsedTender = TedCSVContractNoticeHandler.parse(parsedTender, r);
                        break;
                    case "3": case "6":
                        parsedTender = TedCSVContractAwardHandler.parse(parsedTender, r);
                        break;
                    default:
                        logger.error("No parser implemented for record type {}.", type);
                        throw new UnrecoverableException("Could not create appropriate parsed.");
                }
                
                parsedTenders.add(parsedTender);
            });
        } catch (IOException e) {
            logger.error("Unable to prase CSV {} because of exception", raw.getSourceFileName(), e);
            throw new UnrecoverableException("Unable to prase CSV.", e);
        }
        
        return parsedTenders;
    }

    /**
     * Returns actual version of this parsed manager.
     *
     * @return actual parsed manager version
     */
    @Override
    public final String getVersion() {
        return VERSION;
    }

    /**
     * Parses common data and returns initialized parsed tender.
     *
     * @param record
     *      CSV record
     * @param machineReadableUrl
     *      machine readable url for included publication
     * @return parsed tender
     */
    private ParsedTender parseCommonFormData(final CSVRecord record, final String machineReadableUrl) {
        final ParsedTender parsedTender = new ParsedTender();

        parsedTender
            .addPublication(new ParsedPublication()
                .setIsIncluded(true)
                // columns ID_NOTICE_CN and ID_NOTICE_CAN
                .setSourceId(record.get(0))
                .setSource(PublicationSources.EU_TED_CSV)
                .setMachineReadableUrl(machineReadableUrl)
                // publication year only
                .setPublicationDate(record.get("year"))
                .setSourceFormType(record.get("id_type"))
                .setDispatchDate(record.get("dt_dispatch")))
            .setIsWholeTenderCancelled(TedCSVTenderParserUtils.parseBoolean(record, "cancelled"))
            .addBuyer(parseBuyer(record))
            .setIsOnBehalfOf(TedCSVTenderParserUtils.parseBoolean(record, "b_on_behalf"))
            .setSupplyType(record.get("type_of_contract"))
            .setAddressOfImplementation(new ParsedAddress().addNuts(record.get("tal_location_nuts")))
            .setIsFrameworkAgreement(parseIsFrameworkAgreement(record))
            .setIsDps(TedCSVTenderParserUtils.parseBoolean(record, "b_dyn_purch_syst"))
            .setCpvs(parseCPVs(record))
            .setIsCoveredByGpa(TedCSVTenderParserUtils.parseBoolean(record, "b_gpa"))
            .setAwardCriteria(parseAwardCritera(record))
            .setIsElectronicAuction(TedCSVTenderParserUtils.parseBoolean(record, "b_electronic_auction"))
            .setFundings(parseFundings(record))
            .setSelectionMethod(parseSelectionMethod(record));
        
        return parsedTender;
    }

    /**
     * Parses fundings from the CSV record.
     *
     * @param record
     *      CSV record
     * @return list of parsed funcdings
     */
    private List<ParsedFunding> parseFundings(final CSVRecord record) {
        return Collections.singletonList(new ParsedFunding()
            .setIsEuFund(TedCSVTenderParserUtils.parseBoolean(record, "b_eu_funds")));
    }

    /**
     * Parses isFrameworkAgreement from the CSV record.
     *
     * @param record
     *      CSV record
     * @return "true" only and only if column value is equal to 1 or Y, otherwise "false".
     */
    private String parseIsFrameworkAgreement(final CSVRecord record) {
        String bool = TedCSVTenderParserUtils.parseBoolean(record, "b_fra_agreement");
        if (bool.equals("false")) {
            /*
                Whether there are indications that this notice is actually about a framework agreement, even though it
                has not been marked as such by the buyer (i.e. the buyer possibly forgot to mark the field). Indications
                are the following:

                K - The keyword `framework', in the appropriate language, was found in the title or description of the
                    notice.
                A - Multiple awards were given per one lot, which is legally admissible only in case of framework
                    agreements and dynamic purchasing systems.
                C - Consistency across notices: the contract notice which preceded this notice was marked as a framework
                    agreement.
            */

            String estimation = Optional.ofNullable(record.get("fra_estimated")).orElse("");
            bool = String.valueOf(estimation.contains("K") || estimation.contains("C"));
        }

        return bool;
    }

    /**
     * Parses list of CPVs from the CSV record.
     *
     * @param record
     *      CSV record
     * @return non-empty list of cpvs or null.
     */
    private List<ParsedCPV> parseCPVs(final CSVRecord record) {
        List<ParsedCPV> cpvs = new ArrayList<>();
        // main CPV
        String mainCPV = record.get("cpv");
        if (mainCPV != null) {
            cpvs.add(new ParsedCPV().setIsMain(Boolean.TRUE.toString()).setCode(mainCPV));
        }
        // additionl CPVs
        StringUtils.split(record.get("additional_cpvs"), ",").forEach(code -> {
            cpvs.add(new ParsedCPV().setIsMain(Boolean.FALSE.toString()).setCode(code));
        });
                
        return cpvs.isEmpty() ? null : cpvs;
    }

    /**
     * Parses list of award criteriafrom the CSV record.
     *
     * @param record
     *      CSV record
     * @return non-empty list of award criterion or null
     */
    private List<ParsedAwardCriterion> parseAwardCritera(final CSVRecord record) {
        List<ParsedAwardCriterion> parsedCriteria = new ArrayList<>();
        String type = parseSelectionMethod(record);
        if (type != null) {
            if (type.equals("L")) {
                // lowest price
                parsedCriteria.add(new ParsedAwardCriterion()
                    .setIsPriceRelated(Boolean.TRUE.toString()).setName("Lowest price"));
            } else if (type.equals("M")) {
                // meat
                List<String> criteria = StringUtils.split(record.get("crit_criteria"), "\\|");
                // ?|?|? - weights are unknown or mentioned in criterion name (crit_criteria)
                // eg. 10|80|10 - list of weights
                List<String> weights = StringUtils.split(record.get("crit_weights"), "\\|");

                int index =  0;
                criteria.forEach(name -> {
                    String weight = null;
                    if (index < weights.size() && !weights.get(index).equals("?")) {
                        weight = weights.get(index);
                    } else {
                        // attempts to find weight in criterion name
                        Matcher m = Pattern.compile("([0-9]+)%").matcher(name);
                        if (m.find()) {
                            weight = m.group(1);
                        }
                    }

                    parsedCriteria.add(new ParsedAwardCriterion().setName(name).setWeight(weight));
                });
            }
        }

        return parsedCriteria;
    }

     /**
     * Parses buyer from the given CSV record.
     *
     * @param record
     *      CSV record
     * @return parsed body
     */
    private ParsedBody parseBuyer(final CSVRecord record) {
        
        String country = record.get("iso_country_code");
        
        ParsedBody buyer = new ParsedBody()
            .setName(record.get("cae_name"))            
            .setAddress(new ParsedAddress()
                .setStreet(record.get("cae_address"))
                .setCity(record.get("cae_town"))
                .setPostcode(record.get("cae_postal_code"))
                .setCountry(country))
            .setBuyerType(record.get("cae_type"))
            // Name of the each activity starts with a capital letter so it is necessary to split the string with the
            // comma followed by a capital letter.
            .setMainActivities(StringUtils.split(record.get("main_activity"), ",(?=\\p{Upper})"));
        
        String bodyId = record.get("cae_nationalid");
        if (bodyId != null) {
            buyer.addBodyId(new BodyIdentifier()
                .setId(bodyId)
                .setScope(BodyIdentifier.Scope.valueOf(country))
                .setType(BodyIdentifier.Type.ORGANIZATION_ID));
        }
        
        return buyer;
    }

    /**
     * Parses selection method.
     * 
     * @param record
     *      CSV record
     * @return selection method
     */
    private String parseSelectionMethod(final CSVRecord record) {
        return record.get("crit_code");
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return null;
    }
}
