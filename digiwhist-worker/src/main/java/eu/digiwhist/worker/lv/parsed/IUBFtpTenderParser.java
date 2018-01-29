package eu.digiwhist.worker.lv.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.TenderSize;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses tenders from ftp://open.iub.gov.lv (Latvia).
 *
 * @author Tomas Mrazek
 */
public final class IUBFtpTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1.1";

    /**
     * Pattern that matches time strings. The time string is matched as group.
     */
    private static final String TIME_PATTERN = "([0-9]{1,2}(:?[:\\-\\.][0-9]{1,2})?)";

    /**
     * Pattern that matches time range.
     */
    private static final String TIME_RANGE_PATTERN = new StringBuilder()
            .append(TIME_PATTERN)
            .append("(?:[^0-9]{1,3}| līdz )")
            .append(TIME_PATTERN)
            .toString();

    /**
     * Pattern that matches opening hours with one break.
     */
    private static final String TIME_RANGE_WITH_BREAK_PATTERN = new StringBuilder()
            .append(TIME_RANGE_PATTERN)
            .append("(?:[^0-9]{1,3}| un )")
            .append(TIME_RANGE_PATTERN)
            .toString();

    @Override
    public List<ParsedTender> parse(final RawData raw) {
        Document doc = Jsoup.parse(raw.getSourceData(), "", Parser.xmlParser());

        Element generalNode = JsoupUtils.selectFirst("general", doc);
        Element contactPlacesNode = JsoupUtils.selectFirst("contactplaces", doc);
        Element administrativeInfoNode = JsoupUtils.selectFirst("administrative_info, timelimits", doc);
        Element additionalInfoNode = JsoupUtils.selectFirst("additional_info", doc);
        Element procedureNode = JsoupUtils.selectFirst("procedure, additional", doc);

        if (generalNode == null) {
            generalNode = doc;
        }

        if (administrativeInfoNode == null) {
            administrativeInfoNode = doc;
        }

        if (additionalInfoNode == null) {
            additionalInfoNode = doc;
        }

        if (procedureNode == null) {
            procedureNode = doc;
        }

        String machineReadableUrl = raw.getSourceUrl() + "?" + raw.getSourceFileName();

        ParsedTender parsed = new ParsedTender()
                .setBuyerAssignedId(JsoupUtils.selectText("procurement_code", generalNode))
                .setCpvs(parseCPVs(generalNode))
                .setTitle(JsoupUtils.selectText("general > name, contract_name", doc))
                .setSize(parseSize(doc))
                .setIsOnBehalfOf(hasValue("middleman", doc, "1"))
                .setIsDps(parseIsDPS(doc))
                .setIsFrameworkAgreement(hasValue("contract_expects", doc, "2"))
                .setAddressOfImplementation(parseAddressOfImplementation(doc))
                .setEnvisagedCandidatesCount(JsoupUtils.selectText("businessman_num", doc))
                .setEnvisagedMaxCandidatesCount(JsoupUtils.selectText("max_businessman_num", doc))
                .setIsCoveredByGpa(hasValue("state_procurement", doc, "1"))
                .setExcessiveFrameworkAgreementJustification(JsoupUtils.selectText("basic_contract_argument", doc))
                .setDescription(selectNonEmptyText("description", doc))
                .setHasLots(hasValue("divided_in_parts", doc, "1"))
                .setAreVariantsAccepted(hasValue("variants", doc, "1"))
                .setEstimatedPrice(parseTenderEstimatedPrice(doc))
                .setHasOptions(hasValue("options", doc, "1"))
                .setEstimatedDurationInYears(JsoupUtils.selectText("basic_contract_years", doc))
                .setEstimatedDurationInMonths(JsoupUtils.selectText("basic_contract_moths, contract_months", doc))
                .setEstimatedDurationInDays(JsoupUtils.selectText("contract_days", doc))
                .setEstimatedStartDate(JsoupUtils.selectText("contract_start_date", doc))
                .setEstimatedCompletionDate(JsoupUtils.selectText("contract_end_date", doc))
                .setDeposits(JsoupUtils.selectText("guarantee", doc))
                .setPersonalRequirements(JsoupUtils.selectText("businessman_requirements", doc))
                .setEconomicRequirements(JsoupUtils.selectText("financial_requirements", doc))
                .setTechnicalRequirements(JsoupUtils.selectText("technical_requirements", doc))
                .setIsDocumentsAccessRestricted(
                        hasValue("digital_access", administrativeInfoNode, "1"))
                .setDocumentsLocation(parseDocumentsLocation(administrativeInfoNode))
                .setDocumentsDeadline(parseDocumentsDeadline(doc))
                .setBidDeadline(parseDateTime("submit_", null, administrativeInfoNode))
                .setDocumentsPayable(hasValue("documentation_online", doc, "1"))
                .setDocumentsPayable(hasValue("payment", administrativeInfoNode, "1"))
                .setDocumentsPrice(parsePrice(null, administrativeInfoNode))
                .setAwardDeadline(JsoupUtils.selectText("support_date", administrativeInfoNode))
                .addEligibleBidLanguage(JsoupUtils.selectText("language", doc))
                .addEligibleBidLanguage(JsoupUtils.selectText("other_language", doc))
                .setIsAcceleratedProcedure(hasValue("urgent", administrativeInfoNode, "1"))
                .setAcceleratedProcedureJustification(
                        JsoupUtils.selectText("administrative_info > urgent_text, procedure > argument", doc))
                .setFundings(parseFundings(additionalInfoNode, doc))
                .setAppealBodyName(JsoupUtils.selectText("appeal_name", additionalInfoNode))
                .addOnBehalfOf(parseBody("middleman_", additionalInfoNode))
                .setNationalProcedureType(JsoupUtils.selectText("proc_type", doc))
                .setAcceleratedProcedureJustification(JsoupUtils.selectText("argument", procedureNode))
                .setSelectionMethod(JsoupUtils.selectText("criteria_type", doc))
                .setAwardCriteria(parseAwardCriteria(doc))
                .setIsElectronicAuction(hasValue("auction", doc, "1"))
                .setLots(parseLots(doc))
                .setIsWholeTenderCancelled(
                        hasValue("interrupted_without_results_status", doc, "1"))
                .setCancellationReason(JsoupUtils.selectText("interrupt_info", doc))
                .setSupplyType(parseSupplyType(doc))
                .setModificationReason(JsoupUtils.selectText("changes_reason", doc))
                .setCorrections(parseCorrections(doc))
                .addPublication(parseMainPublication(doc, machineReadableUrl))
                .addPublications(parseOtherPublications(doc));

        ParsedBody buyer = parseBuyer(generalNode, contactPlacesNode);
        parsed.addBuyer(buyer);

        parsed.setFurtherInformationProvider(parseBodyOrElseBuyer(JsoupUtils.selectText("info_places", generalNode),
                buyer,
                () -> parseBody(null, JsoupUtils.selectFirst("contactplace:has(type:contains(Papildu informācija))",
                        contactPlacesNode))));

        parsed.setSpecificationsProvider(parseBodyOrElseBuyer(JsoupUtils.selectText("doc_places", generalNode), buyer,
                () -> parseBody(null, JsoupUtils.selectFirst("contactplace:has(type:contains(Papildu informācija))",
                        contactPlacesNode))));

        parsed.setBidsRecipient(parseBodyOrElseBuyer(JsoupUtils.selectText("submit_places", generalNode), buyer,
                () -> parseBody(null, JsoupUtils.selectFirst("contactplace:has(type:contains(Piedāvājumu iesniegšana))",
                        contactPlacesNode))));

        if (raw.getSourceFileName().contains("473765")) {
            System.out.println("");
        }

        return Collections.singletonList(parsed);
    }

    /**
     * Parse if is DPS.
     *
     * @param doc context
     *
     * @return String or null
     */
    private String parseIsDPS(final Document doc) {
        final String isDPS = JsoupUtils.selectText("contract_expects", doc);

        if (isDPS == null) {
            return null;
        } else if (isDPS.contains("1")) {
            return Boolean.TRUE.toString();
        } else if (!isDPS.isEmpty()) {
            return Boolean.FALSE.toString();
        } else {
            return null;
        }
    }

    /**
     * Parses address of implementation.
     *
     * @param doc parsed document
     *
     * @return address of implementation or null
     */
    private ParsedAddress parseAddressOfImplementation(final Document doc) {
        String place = JsoupUtils.selectText("place", doc);
        String region = JsoupUtils.selectText("region", doc);
        if (place == null && region == null) {
            return null;
        }

        return new ParsedAddress().setState(place).addNuts(region);
    }

    /**
     * Parses documents location.
     *
     * @param context node that includes documents location data
     *
     * @return documents location or null
     */
    private ParsedAddress parseDocumentsLocation(final Element context) {
        String url = JsoupUtils.selectText("url", context);
        if (url == null) {
            return null;
        }

        return new ParsedAddress().setUrl(JsoupUtils.selectText("url", context));
    }

    /**
     * Parses size of tender from the given document.
     *
     * @param doc parsed document
     *
     * @return size or null
     */
    private String parseSize(final Document doc) {
        String size = JsoupUtils.selectText("general > procurement_type", doc);
        String parseSize = hasValue("oj_digital, send_oj", JsoupUtils.selectFirst("administrative_info", doc), "1");
        if (size == null
                && parseSize != null && parseSize.equals(Boolean.TRUE.toString())) {
            size = TenderSize.ABOVE_THE_THRESHOLD.name();
        }

        return size;
    }

    /**
     * Parses tender estimated price from the given document.
     *
     * @param doc parsed document
     *
     * @return price or null
     */
    private ParsedPrice parseTenderEstimatedPrice(final Document doc) {
        ParsedPrice price = parsePrice("general > price_", doc);
        if (price == null) {
            price = parsePrice("basic_contract_price_", doc);
        }
        if (price == null) {
            price = parsePrice("contract_price_", doc);
        }

        return price;
    }

    /**
     * If {@code useBuyerText} is equal to "Iepriekš minētajā (-os) kontaktpunktā (-os)" returns the given {@code buyer}
     * otherwise attempts to create new body with {@code bodyParser}.
     *
     * @param useBuyerText string that includes information whether use the buyer
     * @param buyer        buyer
     * @param bodyParser   function that returns ParsedBody instance
     *
     * @return buyer or new parsed body, null in case that {@code useBuyerText} is null
     */
    private ParsedBody parseBodyOrElseBuyer(final String useBuyerText, final ParsedBody buyer,
                                            final Supplier<ParsedBody> bodyParser) {

        if (useBuyerText == null) {
            return null;
        } else if (useBuyerText.equals("Iepriekš minētajā (-os) kontaktpunktā (-os)")) {
            return buyer;
        }

        return bodyParser.get();
    }

    /**
     * Parses included publication from document.
     *
     * @param doc                parsed document
     * @param machineReadableUrl machine readable url
     *
     * @return included publication
     */
    private ParsedPublication parseMainPublication(final Document doc, final String machineReadableUrl) {
        String dispatchDate = JsoupUtils.selectText("additional_info > contract_approval_date", doc);
        if (dispatchDate == null) {
            dispatchDate = JsoupUtils.selectText("additional_info > notice_send_date", doc);
        }

        //publication[1]
        return new ParsedPublication()
                .setIsIncluded(true)
                .setSource(PublicationSources.LV_IUB_FTP)
                .setSourceTenderId(JsoupUtils.selectText("general > procurement_id", doc))
                .setSourceId(JsoupUtils.selectText("id", doc))
                .setPublicationDate(JsoupUtils.selectText("publication_date, contract_publication_date ", doc))
                .setSourceFormType(JsoupUtils.selectText(":root > type, general > type", doc))
                .setDispatchDate(dispatchDate)
                .setLastUpdate(JsoupUtils.selectText("update_date", doc))
                .setMachineReadableUrl(machineReadableUrl);
    }

    /**
     * Parses all publications from document except included publication.
     *
     * @param doc parsed document
     *
     * @return non-empty list of publications or null
     */
    private List<ParsedPublication> parseOtherPublications(final Document doc) {
        List<ParsedPublication> publications = new ArrayList<>();

        if (hasAnyValue("source_notice", doc).equals(Boolean.TRUE.toString())) {
            publications.add(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSourceId(JsoupUtils.selectText("source_notice", doc)));
        }

        publications = parseList(n -> {
            return new ParsedPublication()
                    .setIsIncluded(false)
                    .setSourceId(JsoupUtils.selectText("oj_num", n))
                    .setPublicationDate(JsoupUtils.selectText("oj_date", n))
                    .setSourceFormType(JsoupUtils.selectText("oj_type", n));
        }, JsoupUtils.select("administrative_info > oj_list > oj", doc), publications);

        publications = parseList(n -> {
            return new ParsedPublication()
                    .setIsIncluded(false)
                    .setPublicationDate(JsoupUtils.selectText("pub_date", n))
                    .setSourceFormType(JsoupUtils.selectText("pub_type", n));
        }, JsoupUtils.select("administrative_info > publication_list > publication", doc), publications);

        if (hasAnyValue("contract_publication_oj_num", doc).equals(Boolean.TRUE.toString())
                || hasAnyValue("contract_publication_oj_date", doc).equals(Boolean.TRUE.toString())) {

            publications.add(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSourceId(JsoupUtils.selectText("contract_publication_oj_num", doc))
                    .setPublicationDate(JsoupUtils.selectText("contract_publication_oj_date", doc)));
        }

        return publications.isEmpty() ? null : publications;
    }

    /**
     * Parses documentsDeadline from the given document.
     *
     * @param doc parsed document
     *
     * @return docuemnts deadline or null
     */
    private String parseDocumentsDeadline(final Document doc) {
        if (doc == null) {
            return null;
        }

        String dateTime = parseDateTime("document_request_", null,
                JsoupUtils.selectFirst("administrative_info", doc));

        if (dateTime == null || dateTime.isEmpty()) {
            String date = JsoupUtils.selectText("dok_req_date", doc, true);
            String time = getLastTime(JsoupUtils.selectText("work_time", doc));

            dateTime = new StringBuilder()
                    .append(date)
                    .append(time.isEmpty() ? "" : " ").append(time)
                    .toString();
        }

        return dateTime;
    }

    /**
     * Returns last time substring (string that matches {@link #TIME_PATTERN}) from the given {@code input} string which
     * matches {@link #TIME_PATTERN}, {@link #TIME_RANGE_PATTERN} or {@link #TIME_RANGE_WITH_BREAK_PATTERN}. In other
     * cases returns null.
     *
     * @param input string with time substrings
     *
     * @return last time substring or empty string
     */
    private String getLastTime(final String input) {
        if (input == null) {
            return "";
        }
        // time range or two time ranges (opening hours with a lunch break), get last time
        if (input.matches(TIME_RANGE_PATTERN)
                || input.matches(TIME_RANGE_WITH_BREAK_PATTERN)) {

            Matcher matcher = Pattern.compile(TIME_PATTERN).matcher(new StringBuilder(input).reverse());
            return matcher.find() ? new StringBuilder(matcher.group(1)).reverse().toString() : "";
        } else if (!input.matches(TIME_PATTERN)) {
            logger.debug("Time string \"{}\" doesn't matches any of time patterns", input);
            return "";
        }

        // input matches TIME_PATTERN
        return input;
    }

    /**
     * Parses all corrections from the given document.
     *
     * @param doc parsed document
     *
     * @return non-empty list of corrections or null
     */
    private List<ParsedCorrigendum> parseCorrections(final Document doc) {
        if (doc == null) {
            return null;
        }

        List<ParsedCorrigendum> corrections = parseList(n -> {
            return new ParsedCorrigendum()
                    .setPlaceOfModifiedText(JsoupUtils.selectText("text_changes_place", n))
                    .setOriginal(JsoupUtils.selectText("text_to_replace", n))
                    .setReplacement(JsoupUtils.selectText("text_replace_with", n));
        }, JsoupUtils.select("text_changes_list > text_changes", doc));

        corrections = parseList(n -> {
            return new ParsedCorrigendum()
                    .setPlaceOfModifiedText(JsoupUtils.selectText("date_changes_place", n))
                    .setOriginalDate(parseDateTime(null, "_to_replace", n))
                    .setReplacementDate(parseDateTime(null, "_replace_with", n));
        }, JsoupUtils.select("date_changes_list > date_changes", doc), corrections);

        corrections = parseList(n -> {
            return new ParsedCorrigendum()
                    .setPlaceOfModifiedText(JsoupUtils.selectText("cp_changes_place", n))
                    .setReplacement(JsoupUtils.selectText("cp_name, cp_reg_num, cp_address, cp_city, cp_zip_code,"
                            + "cp_country, cp_place, cp_person, cp_phone, cp_email, cp_url, cp_url_client", n));
        }, JsoupUtils.select("cp_changes_list > cp_changes", doc), corrections);

        return corrections.isEmpty() ? null : corrections;
    }

    /**
     * Parses price from the given (@code context).
     *
     * @param prefix  prefix of name for the tag that includes price data
     * @param context context
     *
     * @return price or null
     */
    private ParsedPrice parsePrice(final String prefix, final Element context) {
        if (context == null) {
            return null;
        }

        String p = prefix == null ? "" : prefix;

        return new ParsedPrice()
                .setNetAmount(JsoupUtils.selectText(String.format("%1$sexact, %1$sprice", p), context))
                .setMinNetAmount(JsoupUtils.selectText(p + "from", context))
                .setMaxNetAmount(JsoupUtils.selectText(p + "to", context))
                .setCurrency(JsoupUtils.selectText(p + "currency", context))
                .setNetAmountEur(JsoupUtils.selectText(p + "exact_lvl", context));
    }

    /**
     * Parses supplyType.
     *
     * @param doc parsed document
     *
     * @return supply type or null
     */
    private String parseSupplyType(final Document doc) {
        if (hasAnyValue("building_types, building_type", doc).equals(Boolean.TRUE.toString())) {
            return TenderSupplyType.WORKS.name();
        } else if (hasAnyValue("supply_types, supply_type", doc).equals(Boolean.TRUE.toString())) {
            return TenderSupplyType.SUPPLIES.name();
        } else if (hasAnyValue("service_types, service_type", doc).equals(Boolean.TRUE.toString())) {
            return TenderSupplyType.SERVICES.name();
        }

        return null;
    }

    /**
     * Parses CPVs from the given context.
     *
     * @param context context
     *
     * @return non-empty list of CPVs or null
     */
    private List<ParsedCPV> parseCPVs(final Element context) {
        List<ParsedCPV> cpvs = new ArrayList<>();

        if (hasAnyValue("main_cpv > code", context).equals(Boolean.TRUE.toString())) {
            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.TRUE.toString())
                    .setCode(JsoupUtils.selectText("main_cpv > code", context)));
        }

        final Elements additionsCpvs = JsoupUtils.select("cpv_list > cpv, additional_cpv_list > cpv", context);
        for (Element cpv : additionsCpvs) {
            final String cpvCode = JsoupUtils.selectText("code", cpv);

            if (cpvCode != null && !cpvCode.trim().isEmpty()) {
                cpvs.add(new ParsedCPV()
                        .setIsMain(Boolean.FALSE.toString())
                        .setCode(cpvCode));
            }
        }

        return cpvs.isEmpty() ? null : cpvs;
    }

    /**
     * Parses lots.
     *
     * @param doc parsed document
     *
     * @return non-empty list of lots or null
     */
    private List<ParsedTenderLot> parseLots(final Document doc) {
        if (doc == null) {
            return null;
        }

        // base lots information
        List<ParsedTenderLot> lots = parseList(n -> {
            return new ParsedTenderLot()
                    .setTitle(JsoupUtils.selectText("name", n))
                    .setCpvs(parseCPVs(n))
                    .setEstimatedPrice(parsePrice("price_", n))
                    .setEstimatedStartDate(JsoupUtils.selectText("start_date", n))
                    .setEstimatedCompletionDate(JsoupUtils.selectText("end_date", n))
                    .setEstimatedDurationInMonths(JsoupUtils.selectText("months", n))
                    .setEstimatedDurationInDays(JsoupUtils.selectText("days", n))
                    .setLotNumber(JsoupUtils.selectText("nr", n))
                    .setElectronicBidsCount(JsoupUtils.selectText("e_tender_num", n));
        }, JsoupUtils.select("parts > part", doc));

        // other lot information, it is linked by lot number to the base lot. These nodes may appear without base lots
        // information above.
        Elements nodes = JsoupUtils.select("parts_5_list > part_5, part_5_list > part_5", doc);
        if (nodes != null) {
            for (Element n : nodes) {
                String lotNumber = JsoupUtils.selectText("part_nr", n);
                // attempts to find lot by lotNumber in already parsed list of lots
                ParsedTenderLot lot = lots.stream()
                        .filter(ll -> ll.getLotNumber() != null && ll.getLotNumber().equals(lotNumber)).findFirst()
                        .orElse(null);
                // lot with lotNumber doesn't exist, create new one and add them into list;
                if (lot == null) {
                    lot = new ParsedTenderLot();
                    lots.add(lot);
                }

                ParsedPrice bidPrice = parsePrice("price_", n)
                        .setCurrency(JsoupUtils.selectText("currency", n))
                        .setVat(JsoupUtils.selectText("vat_rate", n));

                if (bidPrice.getNetAmount() == null || bidPrice.getNetAmount().isEmpty()) {
                    bidPrice = parsePrice("price_", n)
                            .setCurrency(JsoupUtils.selectText("currency", n))
                            .setVat(JsoupUtils.selectText("vat_rate", n));
                }



                if (bidPrice.getNetAmount() == null || bidPrice.getNetAmount().isEmpty()) {
                    bidPrice
                            .setNetAmount(JsoupUtils.selectText("contract_price_exact", n))
                            .setCurrency(JsoupUtils.selectText("exact_currency", n));
                }

                // update lot with additional information
                lot.setLotNumber(lotNumber)
                        .setContractNumber(JsoupUtils.selectText("contract_nr", n))
                        .setTitle(JsoupUtils.selectText("concluded_contract_name ", n))
                        .setAwardDecisionDate(JsoupUtils.selectText("decision_date ", n))
                        .setBidsCount(JsoupUtils.selectText("tender_num ", n))
                        .setEstimatedPrice(parsePrice("initial_", n))
                        .addBid(new ParsedBid()
                                .setIsWinning(Boolean.TRUE.toString())
                                .setBidders(parseBidders(n))
                                .setPrice(bidPrice)
                                .setIsSubcontracted(hasValue("subcontracts", n, "1"))
                                // hasValue("subcontract_unknown", n, "1");
                                //  subcontractedValue and subcontractedProportion unknown
                                .setSubcontractedValue(parsePrice("subcontract_", n))
                                .setSubcontractedProportion(JsoupUtils.selectText("subcontract_proportion", n)))
                        .setIsAwarded(hasValue("without_results", n, "1"))
                        .setCancellationReason(JsoupUtils.selectText("interrupt_info, without_results_info", n));
            }
        }

        final String price = JsoupUtils.selectText("document > price", doc);
        if (lots.isEmpty() && price != null) {
            lots.add(new ParsedTenderLot()
                    .addBid(new ParsedBid()
                            .setPrice(new ParsedPrice()
                                    .setNetAmount(price)
                                    .setCurrency(JsoupUtils.selectText("document > currency", doc)))));
        }

        return lots.isEmpty() ? null : lots;
    }

    /**
     * Parses list of <T> instances from the given list of {@code nodes}. If parser returns null it doesn't added into
     * result list.
     *
     * @param <T>    class of list items
     * @param parser item parser
     * @param nodes  list of nodes that include data for item parsing
     * @param preset parsed non-null items will be added into this list, if is null new list is created
     *
     * @return list of parsed items or empty list
     */
    private <T> List<T> parseList(final Function<Element, T> parser, final Elements nodes,
                                  final List<T> preset) {

        final List<T> list = preset == null ? new ArrayList<>() : preset;

        if (nodes == null || nodes.isEmpty()) {
            return list;
        }

        nodes.stream().forEach((node) -> {
            T item = parser.apply(node);
            if (item != null) {
                list.add(item);
            }
        });

        return list;
    }

    /**
     * Parses list of <T> instances from the given list of {@code nodes}.
     *
     * @param <T>    class of list items
     * @param parser item parser
     * @param nodes  list of nodes that include data for item parsing
     *
     * @return list of parsed items or empty list
     */
    private <T> List<T> parseList(final Function<Element, T> parser, final Elements nodes) {
        return parseList(parser, nodes, null);
    }

    /**
     * Parses list of bidders from the given {@code context}.
     *
     * @param context context
     *
     * @return non-empty list of bidders or null
     */
    private List<ParsedBody> parseBidders(final Element context) {
        List<ParsedBody> bidders =
                parseList(n -> parseBody("winner_", n), JsoupUtils.select("winner_list > winner", context));

        return bidders.isEmpty() ? null : bidders;
    }

    /**
     * Parses lits of award criteria from the given {@code context}.
     *
     * @param context context
     *
     * @return non-empty list of award criteria or null
     */
    private List<ParsedAwardCriterion> parseAwardCriteria(final Element context) {
        List<ParsedAwardCriterion> criteria = parseList(n -> new ParsedAwardCriterion()
                .setName(JsoupUtils.selectText("name", n))
                .setWeight(JsoupUtils.selectText("points", n)),
                JsoupUtils.select("criteria_list > criteria", context));

        final String anotherCriteria = JsoupUtils.selectText("criteria_type", context);

        if (anotherCriteria != null) {
            criteria.add(new ParsedAwardCriterion()
                    .setName(anotherCriteria)
                    .setWeight(String.valueOf(100))
                    .setIsPriceRelated(Boolean.TRUE.toString()));
        }

        return criteria.isEmpty() ? null : criteria;
    }

    /**
     * Parses list of fundings.
     *
     * @param additionalInfoNode <additional_ifno> node
     * @param doc                whole doc
     *
     * @return non-empty list of fundings or null
     */
    private List<ParsedFunding> parseFundings(final Element additionalInfoNode, final Element doc) {
        final String isEuFund = hasValue("eu_fund", doc, "1");

        List<ParsedFunding> fundings = parseList(n -> new ParsedFunding()
                        .setIsEuFund(isEuFund)
                        .setProgramme(JsoupUtils.selectText("eu_long, eu_short", n)),
                JsoupUtils.select("eu_list > eu", doc));

        final String isNotEuFund = hasValue("eu_fund", doc, "0");

        if (isNotEuFund != null && isNotEuFund.equals(Boolean.TRUE.toString())) {
            fundings.add(new ParsedFunding()
                    .setIsEuFund("false"));
        }

        return fundings.isEmpty() ? null : fundings;
    }

    /**
     * Parses datetime from the given {@code context}.
     *
     * @param prefix  prefix of name for the tag that includes datetime data
     * @param suffix  suffix of name for the tag that includes price data
     * @param context context
     *
     * @return datetime string or null
     */
    private String parseDateTime(final String prefix, final String suffix, final Element context) {
        if (context == null) {
            return null;
        }

        String tagNamePattern = new StringBuilder()
                .append(Optional.ofNullable(prefix).orElse(""))
                .append("%s")
                .append(Optional.ofNullable(suffix).orElse(""))
                .toString();

        String date = JsoupUtils.selectText(String.format(tagNamePattern, "date"), context, true);
        String hour = JsoupUtils.selectText(String.format(tagNamePattern, "hour"), context, true);
        String minute = JsoupUtils.selectText(String.format(tagNamePattern, "minute"), context, true);

        if (minute != null && minute.length() == 1) {
            minute = "0" + minute;
        }

        String dateTime = new StringBuilder()
                .append(date)
                .append(hour.isEmpty() ? "" : " ").append(hour)
                .append(minute.isEmpty() ? "" : ":").append(minute)
                .toString();

        return dateTime.isEmpty() ? null : dateTime;
    }

    /**
     * Parses buyer from the given {@code context}.
     *
     * @param buyerElement context
     * @param contact      context
     *
     * @return buyer or null
     */
    private ParsedBody parseBuyer(final Element buyerElement, final Element contact) {
        if (buyerElement == null) {
            return null;
        }

        ParsedBody buyer = new ParsedBody()
                .setName(JsoupUtils.selectText("authority_name", buyerElement))
                .setAddress(parseAddress(null, buyerElement))
                .setContactName(JsoupUtils.selectText("person", contact))
                .setContactPoint(JsoupUtils.selectText("name", contact))
                .setPhone(JsoupUtils.selectText("phone", contact));

        String bodyId = JsoupUtils.selectText("authority_reg_num", buyerElement);
        if (bodyId != null && !bodyId.isEmpty()) {
            buyer.addBodyId(new BodyIdentifier()
                    .setId(bodyId)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                    .setScope(BodyIdentifier.Scope.LV));
        }

        bodyId = JsoupUtils.selectText("author_id", buyerElement);
        if (bodyId != null && !bodyId.isEmpty()) {
            buyer.addBodyId(new BodyIdentifier()
                    .setId(bodyId)
                    .setType(BodyIdentifier.Type.SOURCE_ID)
                    .setScope(BodyIdentifier.Scope.LV));
        }

        bodyId = JsoupUtils.selectText("authority_id", buyerElement);
        if (bodyId != null && !bodyId.isEmpty()) {
            buyer.addBodyId(new BodyIdentifier()
                    .setId(bodyId)
                    .setType(BodyIdentifier.Type.SOURCE_ID)
                    .setScope(BodyIdentifier.Scope.LV));
        }

        return buyer;
    }

    /**
     * Parses body from the given {@code context}.
     *
     * @param prefix  prefix of name for the tags that include body data
     * @param context context
     *
     * @return body or null
     */
    private ParsedBody parseBody(final String prefix, final Element context) {
        if (context == null) {
            return null;
        }

        String p = prefix == null ? "" : prefix;

        ParsedBody body = new ParsedBody()
                .setAddress(parseAddress(p, context)
                        .setUrl(JsoupUtils.selectText(String.format("%1$sweb, %1$surl", p), context)))
                .setName(JsoupUtils.selectText(p + "name", context))
                .setContactPoint(JsoupUtils.selectText(p + "place", context))
                .setContactName(JsoupUtils.selectText(p + "person", context))
                .setPhone(JsoupUtils.selectText(p + "phone", context))
                .setEmail(JsoupUtils.selectText(p + "email", context));

        String organizationId = JsoupUtils.selectText(p + "reg_num", context);
        if (organizationId != null && !organizationId.isEmpty()) {
            body.addBodyId(new BodyIdentifier()
                    .setId(organizationId)
                    .setScope(BodyIdentifier.Scope.LV)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID));
        }

        return body;
    }

    /**
     * Parses address from the given {@code context}.
     *
     * @param prefix  prefix of name for the tag that includes address data
     * @param context context
     *
     * @return address or null
     */
    private ParsedAddress parseAddress(final String prefix, final Element context) {
        if (context == null) {
            return null;
        }

        String p = prefix == null ? "" : prefix;

        return new ParsedAddress()
                .setStreet(JsoupUtils.selectText(p + "address", context))
                .setCity(JsoupUtils.selectText(p + "city", context))
                .setPostcode(JsoupUtils.selectText(p + "zip_code", context))
                .setCountry(JsoupUtils.selectText(p + "country", context))
                .setUrl(JsoupUtils.selectText(p + "url", context));
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "LV";
    }

    /**
     * Check if any element gotten by selector has value.
     *
     * @param selector selector to find elements
     * @param element  element to search in
     *
     * @return String
     */
    private String hasAnyValue(final String selector, final Element element) {
        final Elements possibleResults = JsoupUtils.select(selector, element);

        for (Element possibleResult : possibleResults) {
            if (!possibleResult.text().trim().isEmpty()) {
                return Boolean.TRUE.toString();
            }
        }

        return Boolean.FALSE.toString();
    }

    /**
     * Check if any element gotten by selector has expected value.
     *
     * @param selector      selector to find elements
     * @param element       element to search in
     * @param expectedValue expected value
     *
     * @return String or null
     */
    private String hasValue(final String selector, final Element element, final String expectedValue) {
        if (element == null) {
            return null;
        }

        final Elements possibleResults = JsoupUtils.select(selector, element);

        for (Element possibleResult : possibleResults) {
            if (possibleResult.text().contains(expectedValue)) {
                return Boolean.TRUE.toString();
            } else if (!possibleResult.text().trim().isEmpty()) {
                return Boolean.FALSE.toString();
            }
        }

        return null;
    }

    /**
     * Select value that is not empty.
     * @param selector selector
     * @param element element to select from
     * @return String or null
     */
    private String selectNonEmptyText(final String selector, final Element element) {
        if (element == null) {
            return null;
        }

        final Elements possibleResults = JsoupUtils.select(selector, element);

        for (Element possibleResult : possibleResults) {
            if (!possibleResult.text().trim().isEmpty()) {
                return possibleResult.text();
            }
        }

        return null;
    }
}
