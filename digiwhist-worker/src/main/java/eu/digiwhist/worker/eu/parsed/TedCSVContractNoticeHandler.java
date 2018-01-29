package eu.digiwhist.worker.eu.parsed;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.csv.CSVRecord;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.StringUtils;

/**
 * The handler for TED contract notice CVS record.
 *
 * @author Tomas Mrazek
 */
public final class TedCSVContractNoticeHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedCSVContractNoticeHandler() {
        throw new AssertionError();
    }

    /**
     * Parses contract notice CSV record specific data.
     *
     * @param parsedTender
     *      parsed tender with common data
     * @param record
     *      CSV record
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final CSVRecord record) {

        parsedTender
            .setEnvisagedCandidatesCount(parseEnvisagedCandidatesCount(record))
            .setMaxFrameworkAgreementParticipants(record.get("fra_number_max_operators"))
            .setAreVariantsAccepted(TedCSVTenderParserUtils.parseBoolean(record, "b_variants"))
            .setEstimatedPrice(TedCSVTenderParserUtils.parseTenderPrice(record))
            .setHasOptions(TedCSVTenderParserUtils.parseBoolean(record, "b_options"))
            .setEstimatedDurationInMonths(record.get("duration"))
            .setEstimatedStartDate(record.get("contract_start"))
            .setEstimatedCompletionDate(record.get("contract_completion"))
            .setProcedureType(record.get("procedure"))            
            .setEnvisagedMinCandidatesCount(record.get("env_min_operators"))
            .setEnvisagedMaxCandidatesCount(record.get("env_max_operators"))            
            .setBidDeadline(record.get("dt_applications"))
            .setEligibleBidLanguages(parseEligibleBidLanguages(record));

        String coctractAwardReference = record.get("future_can_id");
        if (coctractAwardReference != null) {
            parsedTender.addPublication(new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.EU_TED_CSV)
                .setSourceId(coctractAwardReference));
        }

        return parsedTender;
    }

    /**
     * Parses envisaged candidates count from the given CSV {@code record}.
     *
     * @param record
     *      CSV record
     * @return count of envisaged candidates or null
     */
    private static String parseEnvisagedCandidatesCount(final CSVRecord record) {
        String count = record.get("env_operators");
        if (count == null) {
            count = record.get("fra_number_operators");
        }
        
        return count;
    }

    /**
     * Parses list of eligible bid languages from the CSV record.
     *
     * @param record
     *      CSV record
     * @return non-empty list of eligible bid languages or null
     */
    private static List<String> parseEligibleBidLanguages(final CSVRecord record) {
        List<String> langs = new ArrayList<>();

        Arrays.asList("b_language_any_ec", "admin_languages_tender", "admin_other_languages_tender").forEach(column -> {
            langs.addAll(StringUtils.split(record.get(column), "\\|"));
        });
        
        return langs.isEmpty() ? null : langs;
    }
}
