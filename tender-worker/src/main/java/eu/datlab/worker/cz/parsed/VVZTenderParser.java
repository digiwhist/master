package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getFieldValue;
import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getSelectedOptionValue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * This class is responsible for managing parsing of the raw tenders downloaded from Vestnik.
 */
public final class VVZTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    static final String VESTNIK_SOURCE_URL = PublicationSources.CZ_VESTNIK;
    static final String TED_SOURCE_URL = PublicationSources.EU_TED;

    private static final Set<String> SUPPORTED_FORMS = new HashSet<String>(
            Arrays.asList("F01", "F02", "F03", "F04", "F05", "F06", "F12", "F13", "F14", "F15", "F20", "CZ01", "CZ02",
                    "CZ03", "CZ04"));

    private static final Logger logger = LoggerFactory.getLogger(VVZTenderParser.class);

    /**
     * Parses the given raw tender object.
     *
     * @param rawTender
     *         raw tender to be parsed
     *
     * @return list containing one tender with details parsed from source or
     * empty list, if the tender has not been parsed
     * @throws UnrecoverableException
     *         if no parser could be created for given raw tender
     */
    @Override
    public List<ParsedTender> parse(final RawData rawTender) {
        final String formCode = parseFormType(rawTender);
        logger.info("Trying to parse form {} from {}.", formCode, rawTender.getSourceUrl());

        final Document form = Jsoup.parse(rawTender.getSourceData());
        // parse common attributes
        ParsedTender tender = parseCommonTenderInfo(form, rawTender);

        // parse form specific attributes with appropriate form handler
        switch (formCode) {
            case "F01":
                tender = VVZFormF01Handler.parseFormAttributes(tender, form);
                break;
            case "F02":
                tender = VVZFormF02Handler.parseFormAttributes(tender, form);
                break;
            case "F03":
                tender = VVZFormF03Handler.parseFormAttributes(tender, form);
                break;
            case "F04":
                tender = VVZFormF04Handler.parseFormAttributes(tender, form);
                break;
            case "F05":
                tender = VVZFormF05Handler.parseFormAttributes(tender, form);
                break;
            case "F06":
                tender = VVZFormF06Handler.parseFormAttributes(tender, form);
                break;
            case "F12":
                tender = VVZFormF12Handler.parseFormAttributes(tender, form);
                break;
            case "F13":
                tender = VVZFormF13Handler.parseFormAttributes(tender, form);
                break;
            case "F14":
                tender = VVZFormF14Handler.parseFormAttributes(tender, form);
                break;
            case "F15":
                tender = VVZFormF15Handler.parseFormAttributes(tender, form);
                break;
            case "F20":
                tender = VVZFormF20Handler.parseFormAttributes(tender, form);
                break;
            case "CZ01":
                tender = VVZFormCZ01Handler.parseFormAttributes(tender, form);
                break;
            case "CZ02":
                tender = VVZFormCZ02Handler.parseFormAttributes(tender, form);
                break;
            case "CZ03":
                tender = VVZFormCZ03Handler.parseFormAttributes(tender, form);
                break;
            case "CZ04":
                tender = VVZFormCZ04Handler.parseFormAttributes(tender, form);
                break;
            default:
                logger.warn("No handler found for form code: {}", formCode);
                break;
        }
        return new ArrayList<>(Collections.singletonList(tender));
    }

    /**
     * Returns actual version of this parsed manager.
     *
     * @return actual parsed manager version
     */
    @Override
    public String getVersion() {
        return VERSION;
    }

    /**
     * Returns form type (form number, eg. 2, 3, 6,...) from metadata in {@code rawTender}.
     *
     * @param rawTender
     *         raw tender with downloaded source data and metadata
     *
     * @return form number which specifies the form type (eg. 2, 3, 6,...)
     */
    protected static String parseFormType(final RawData rawTender) {
        return (String) rawTender.getMetaData().get("formNumber");
    }

    /**
     * Gets form publication date from the form header.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return publication date of given parsed form
     */
    protected static String parseFormPublicationDate(final Document form) {
        return VVZTenderParserUtils.parseInfoFromHeader(form, "Datum uveřejnění ve VVZ");
    }

    /**
     * Parses info about publication of this form in TED. In case the form has
     * been sent, but not published yet, this will only be information about the
     * dispatch date. If the form has also been published, it's id and
     * publication date is provided.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return parsed info about publication of this form in TED or null if no
     * such info found.
     */
    protected static ParsedPublication parseThisPublicationInTed(final Document form) {
        final String dispatchDate = VVZTenderParserUtils.parseInfoFromHeader(form, "Datum odeslání do TED");
        final String publicationDate = VVZTenderParserUtils.parseInfoFromHeader(form, "Datum uveřejnění v TED");
        final String formId = VVZTenderParserUtils.parseInfoFromHeader(form, "Číslo oznámení TED");

        if (dispatchDate != null || publicationDate != null || formId != null) {
            return new ParsedPublication().setIsIncluded(false)
                    .setSource(TED_SOURCE_URL)
                    .setDispatchDate(dispatchDate)
                    .setPublicationDate(publicationDate)
                    .setSourceId(formId);
        }
        return null;
    }

    /**
     * Checks whether the passed form code is supported by the parsed.
     *
     * @param formCode
     *         form number which specifies the form type
     *
     * @return true if specified form is supported and can be parsed, false otherwise
     */
    private static boolean isFormSupported(final String formCode) {
        return SUPPORTED_FORMS.contains(formCode);
    }

    /**
     * Parses tender attributes that are common for all the form types (eg. primary publication etc.)
     *
     * @param form
     *         parsed document for the source HTML page
     * @param rawTender
     *         raw tender object
     *
     * @return tender with parsed information from common sections
     */
    private static ParsedTender parseCommonTenderInfo(final Document form, final RawData rawTender) {
        if (form == null) {
            logger.error("Could not start parsing tender data. Source data could not be parsed.");
            throw new UnrecoverableException("Source data could not be parsed.");
        }
        return new ParsedTender().setPublications(parsePublications(form, rawTender));
    }

    /**
     * Parses info about this publication and all the related ones in Vestnik.
     * Source HTML for results of related Vestnik publications search is stored
     * within the {@code rawTender} object.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param rawTender
     *         raw tender with downloaded source data and metadata
     *
     * @return parsed primary publication and all the related ones that are available in Vestnik
     */
    private static List<ParsedPublication> parsePublications(final Document form, final RawData rawTender) {
        final List<ParsedPublication> publications = new ArrayList<>();

        final String formCode = parseFormType(rawTender);
        final String sourceUrl = rawTender.getSourceUrl().toString();

        final HashMap<String, String> additionalUrls = (HashMap<String, String>) rawTender.getMetaData()
                .get("additionalUrls");

        // THIS PUBLICATION
        publications.add(parseThisPublication(form, formCode, sourceUrl));

        // THIS PUBLICATION IN TED (EU FORM ONLY)
        if (isEuropeanForm(formCode)) {
            publications.add(parseThisPublicationInTed(form));
        }

        // RELATED PUBLICATIONS IN VESTNIK (using searching for related forms)
        if (additionalUrls == null || additionalUrls.isEmpty()) {
            logger.error(
                    "Unable to parse related publications from Vestnik. Related forms page not found in metadata.");
        } else {
            final String formId = parseFormId(form);
            final Document relatedPublicationsForm = Jsoup.parse(additionalUrls.values().iterator().next(),
                    VESTNIK_SOURCE_URL);
            publications.addAll(parseRelatedVestnikPublications(relatedPublicationsForm, formId));
        }
        return publications;
    }

    /**
     * Parses info about this publication.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param formCode
     *         form number which specifies the form type
     * @param sourceUrl
     *         form source URL
     *
     * @return this publication with parsed details
     */
    private static ParsedPublication parseThisPublication(final Document form, final String formCode,
            final String sourceUrl) {
        return new ParsedPublication().setIsIncluded(true)
                .setSource(VESTNIK_SOURCE_URL)
                .setSourceFormType(formCode)
                .setSourceId(parseFormId(form))
                .setSourceTenderId(parseTenderId(form))
                .setPublicationDate(parseFormPublicationDate(form))
                .setHumanReadableUrl(sourceUrl)
                .setDispatchDate(parseFormSendDate(form, formCode))
                .setBuyerAssignedId(parseBuyerAssignedId(form));
    }

    /**
     * Gets form source ID (form evidence number in Vestnik).
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return form source ID (called form evidence number in Vestnik)
     */
    private static String parseFormId(final Document form) {
        return VVZTenderParserUtils.parseInfoFromHeader(form, "Evidenční číslo formuláře");
    }

    /**
     * Parses tender id.
     *
     * @param form
     *         form html
     *
     * @return tender id
     */
    static String parseTenderId(final Document form) {
        return VVZTenderParserUtils.parseInfoFromHeader(form, "Evidenční číslo zakázky");
    }

    /**
     * Gets section I html.
     *
     * @param form
     *         form html
     *
     * @return section I html
     */
    static Element getSectionI(final Document form) {
        return form.select("div#Body").first();
    }

    /**
     * Gets section II html.
     *
     * @param form
     *         form html
     *
     * @return section II html
     */
    static Element getSectionII(final Document form) {
        return form.select("div#Contract, div#ContractCZ").first();
    }

    /**
     * Gets section III html.
     *
     * @param form
     *         form html
     *
     * @return section III html
     */
    static Element getSectionIII(final Document form) {
        return form.select("div#LeftI, div#ProcedureCZ").first();
    }

    /**
     * Gets section IV html.
     *
     * @param form
     *         form html
     *
     * @return section IV html
     */
    static Element getSectionIV(final Document form) {
        return form.select("div#Procedure, div#Procedure2").first();
    }

    /**
     * Gets section V html.
     *
     * @param form
     *         form html
     *
     * @return section V html
     */
    static Element getSectionV(final Document form) {
        return form.select("div#AwardsOfContract").first();
    }

    /**
     * Gets section VI html.
     *
     * @param form
     *         form html
     *
     * @return section VI html
     */
    static Element getSectionVI(final Document form) {
        return form.select("div#ComplementaryInfo").first();
    }

    /**
     * Gets section V html for CZ forms.
     *
     * @param form
     *         form html
     *
     * @return section V html for CZ forms
     */
    static Element getCZSectionV(final Document form) {
        return getSectionVI(form);
    }


    // ---------------------------------
    // SUBSECTION VI.5) (CZ V.2)
    // ---------------------------------

    /**
     * Parses form send date.
     *
     * @param form
     *         form html
     * @param formCode
     *         form code
     *
     * @return form send date
     */
    private static String parseFormSendDate(final Document form, final String formCode) {
        if (isEuropeanForm(formCode)) {
            final Element publicationDateDiv = form.select(
                    "div#ComplementaryInfo div:contains(Datum odeslání tohoto oznámení:)").first();
            if (publicationDateDiv != null) {
                final String[] textParts = publicationDateDiv.text().split(":");
                if (textParts.length > 1) {
                    return textParts[1].trim();
                }
            }
        } else {
            // czech forms
            return VVZTenderParserUtils.getFieldValue(form, "ComplementaryInfo\\.DispatchNotice");
        }
        return null;
    }

    // =================================
    // SECTION I
    // =================================

    // ---------------------------------
    // SUBSECTION I.1.1)
    // ---------------------------------

    /**
     * Parses buyer assigned id.
     *
     * @param form
     *         form html
     *
     * @return buyer assigned id
     */
    private static String parseBuyerAssignedId(final Document form) {
        return VVZTenderParserUtils.getFieldValue(form, "Contract\\.ReferenceNumber");
    }

    // ---------------------------------
    // SUBSECTION I.3) (CZ I.2)
    // ---------------------------------

    /**
     * Parses documents location.
     *
     * @param root
     *         html for parsing
     *
     * @return documents location
     */
    static ParsedAddress parseDocumentsLocation(final Element root) {
        final String documentsUrl = VVZTenderParserUtils.getFieldValue(root, "Body\\.UrlDocument");
        if (StringUtils.isNotEmpty(documentsUrl)) {
            return new ParsedAddress().setUrl(documentsUrl);
        }
        return null;
    }

    // =================================
    // SECTION II
    // =================================

    // ---------------------------------
    // SUBSECTION II.1.1)
    // ---------------------------------

    /**
     * Parses tender title.
     *
     * @param sectionII
     *         section II html
     *
     * @return tender title
     */
    static String parseTenderTitle(final Element sectionII) {
        return VVZTenderParserUtils.getFieldValue(sectionII, ".*\\.Title$");
    }

    // ---------------------------------
    // SUBSECTION II.1.3)
    // ---------------------------------

    /**
     * Parses supply type.
     *
     * @param sectionII
     *         section II html
     *
     * @return supply type
     */
    static String parseSupplyType(final Element sectionII) {
        return VVZTenderParserUtils.getCheckedInputValue(sectionII, ".*\\.ContractType$");
    }

    // ---------------------------------
    // SUBSECTION II.1.6) (CZ II.4.1)
    // ---------------------------------

    /**
     * Parses whether tender has lots.
     *
     * @param sectionII
     *         section II html
     *
     * @return true if tender has lots
     */
    static String parseHasLots(final Element sectionII) {
        return VVZTenderParserUtils.getCheckedInputValue(sectionII, ".*\\.LotDivisionType$");
    }

    // ---------------------------------
    // SUBSECTION II.2.1) (CZ II.5.1)
    // ---------------------------------

    /**
     * Parses lot title.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot title
     */
    static String parseLotTitle(final Element lotHtml) {
        return getFieldValue(lotHtml, ".*Title");
    }

    /**
     * Parses lot number.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot number
     */
    static String parseLotNumber(final Element lotHtml) {
        return getFieldValue(lotHtml, ".*LotNumber");
    }

    // ---------------------------------
    // SUBSECTION II.2.4) and VII.1.4) (CZ II.5.2, IV.2.1)
    // ---------------------------------

    /**
     * Parses lot description.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot description
     */
    static String parseLotDescription(final Element lotHtml) {
        return getFieldValue(lotHtml, ".*ShortDesc$");
    }

    // ---------------------------------
    // SUBSECTION II.2.10) (CZ II.1.4)
    // ---------------------------------

    /**
     * Parses whether the variants are accepted.
     *
     * @param root
     *         html for parsing
     *
     * @return true if the variants are accepted
     */
    static String parseAreVariantsAcceptedFromCheckbox(final Element root) {
        return BooleanUtils.toStringTrueFalse(VVZTenderParserUtils.isInputFieldChecked(root, ".*AcceptedVariants"));
    }

    // ---------------------------------
    // SUBSECTION II.2.13) (CZ II.1.8)
    // ---------------------------------

    /**
     * Parses EU funding info.
     *
     * @param root
     *         html for parsing
     *
     * @return EU funding
     */
    static ParsedFunding parseEuFunding(final Element root) {
        final String isEuFunded = VVZTenderParserUtils.getCheckedInputValue(root, ".*EUProgrRelated");
        if (isEuFunded != null && (isEuFunded.equalsIgnoreCase("true") || isEuFunded.equalsIgnoreCase(
                "EU_PROGR_RELATED"))) {
            return new ParsedFunding().setIsEuFund(Boolean.TRUE.toString())
                    .setProgramme(getFieldValue(root, ".*EUProgrRelatedDesc"));
        }
        return null;
    }

    // =================================
    // SECTION IV
    // =================================

    // ---------------------------------
    // SUBSECTION IV.1.1) (CZ III.1.1)
    // ---------------------------------

    /**
     * Parses procedure type.
     *
     * @param root
     *         html segment for parsing
     *
     * @return procedure type
     */
    static String parseProcedureType(final Element root) {
        return VVZTenderParserUtils.getCheckedInputValue(root, ".*\\.ProcedureType");
    }

    // ---------------------------------
    // SUBSECTION IV.1.6) (CZ III.2.2)
    // ---------------------------------

    /**
     * Parses whether electronic auction is used.
     *
     * @param root
     *         html for parsing
     *
     * @return true if electronic auction is used
     */
    static String parseIsElectronicAuction(final Element root) {
        return BooleanUtils.toStringTrueFalse(VVZTenderParserUtils.isInputFieldChecked(root, ".*\\.EAuctionUsed$"));
    }

    // =================================
    // SECTION V (CZ SECTION IV)
    // =================================

    /**
     * Gets lots awards htmls.
     *
     * @param form
     *         form html
     *
     * @return lots awards htmls
     */
    static Elements getLotsAwardsHtmls(final Document form) {
        return form.select("div[model~=.*AwardsOfContract\\[\\d+\\]$]");
    }

    /**
     * Parses lot contract number.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot contract number
     */
    static String parseLotAwardContractNumber(final Element lotAwardHtml) {
        return getFieldValue(lotAwardHtml, ".*\\.ContractNumber$");
    }

    /**
     * Parses lot number.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot number
     */
    static String parseLotAwardNumber(final Element lotAwardHtml) {
        return getFieldValue(lotAwardHtml, ".*\\.LotNumber$");
    }

    /**
     * Parses lot title.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot title
     */
    static String parseLotAwardTitle(final Element lotAwardHtml) {
        return getFieldValue(lotAwardHtml, ".*\\.Title$");
    }

    /**
     * Parses whether lot is awarded.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return true if the lot is awarded
     */
    static String parseIsLotAwarded(final Element lotAwardHtml) {
        return VVZTenderParserUtils.getCheckedInputValue(lotAwardHtml, ".*\\.IsAwarded$");
    }

    // ---------------------------------
    // SUBSECTION V.1)
    // ---------------------------------

    /**
     * Parses lot cancellation reason.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot cancellation reason
     */
    static String parseLotAwardCancellationReason(final Element lotAwardHtml) {
        return VVZTenderParserUtils.getCheckedInputValue(lotAwardHtml, ".*\\.ProcurementUnsuccessfull$");
    }

    // ---------------------------------
    // SUBSECTION V.2.1) (V.3.1) (CZ IV.3.1)
    // ---------------------------------

    /**
     * Parses lot contract signature date.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot contract signature date
     */
    static String parseLotAwardContractSignatureDate(final Element lotAwardHtml) {
        return getFieldValue(lotAwardHtml, ".*\\.DateConclusionContract$");
    }

    // ---------------------------------
    // SUBSECTION V.2.2) (V.3.2) (CZ IV.3.2)
    // ---------------------------------

    /**
     * Parses lot bids count.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot bids count
     */
    static String parseLotAwardBidsCount(final Element lotAwardHtml) {
        return getFieldValue(lotAwardHtml, ".*\\.TendersReceived$");
    }

    /**
     * Parses lot electronic bids count.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot electronic bids count
     */
    static String parseLotAwardElectronicBidsCount(final Element lotAwardHtml) {
        return getFieldValue(lotAwardHtml, ".*\\.TendersReceivedEMeans$");
    }

    /**
     * Parses whether lot is awarded to group of suppliers.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return true if lot is awarded to group of suppliers
     */
    static String parseLotAwardBidIsConsortium(final Element lotAwardHtml) {
        return VVZTenderParserUtils.getCheckedInputValue(lotAwardHtml, ".*\\.AwardedToGroup$");
    }

    // ---------------------------------
    // SUBSECTION V.2.3) (V.3.3) (CZ IV.3.3)
    // ---------------------------------

    /**
     * Parses lot winners.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot winners
     */
    static List<ParsedBody> parseLotAwardWinners(final Element lotAwardHtml) {
        final List<ParsedBody> winners = new ArrayList<>();
        final Elements winnersHtmls = getLotAwardWinnersHtmls(lotAwardHtml);

        for (Element winnerHtml : winnersHtmls) {
            final ParsedBody winner = parseBody(winnerHtml);
            winner.setIsSme(parseLotAwardWinnerIsSME(winnerHtml));
            winners.add(winner);
        }

        // set first winner as leader if there are more winners
        if (winners.size() > 1) {
            winners.get(0).setIsLeader(Boolean.TRUE.toString());
        }

        return winners;
    }

    /**
     * Gets lot winners htmls.
     *
     * @param sectionV
     *         section V html
     *
     * @return lot winners htmls
     */
    private static Elements getLotAwardWinnersHtmls(final Element sectionV) {
        return sectionV.select("div[model~=.*\\.ContractorList\\[\\d+\\]$]");
    }

    /**
     * Parses whether lot winner is SME.
     *
     * @param bidderHtml
     *         bidder html
     *
     * @return true if lot winner is SME
     */
    private static String parseLotAwardWinnerIsSME(final Element bidderHtml) {
        return VVZTenderParserUtils.getCheckedInputValue(bidderHtml, ".*\\.Sme$");
    }

    // =================================
    // SECTION VI (CZ V)
    // =================================

    /**
     * Parses additional tender info.
     *
     * @param root
     *         html segment for parsing
     *
     * @return additional tender info
     */
    static String parseAdditionalInfo(final Element root) {
        return getFieldValue(root, ".*\\.AdditionalInfo");
    }

    // =================================
    // SECTION VII (CZ VI)
    // =================================

    // ---------------------------------
    // SUBSECTION VII.1) (CZ VI.1)
    // ---------------------------------

    /**
     * Parses text corrections.
     *
     * @param changeSection
     *         change html
     *
     * @return text corrections
     */
    static List<ParsedCorrigendum> parseTextCorrections(final Element changeSection) {
        final List<ParsedCorrigendum> textCorrections = new ArrayList<>();

        final Elements textChangesHtmls = getTextChangesHtmls(changeSection);
        for (Element textChangeHtml : textChangesHtmls) {
            final String sectionNumber = parseCorrectionSectionNumber(textChangeHtml);
            if (StringUtils.isNotEmpty(sectionNumber)) {
                final ParsedCorrigendum corrigendum = new ParsedCorrigendum().setSectionNumber(sectionNumber)
                        .setLotNumber(parseCorrectionLotNumber(textChangeHtml))
                        .setPlaceOfModifiedText(parseCorrectionPlace(textChangeHtml))
                        .setOriginal(parseCorrectionOriginalText(textChangeHtml))
                        .setReplacement(parseCorrectionReplacementText(textChangeHtml));
                textCorrections.add(corrigendum);
            }
        }
        return textCorrections;
    }

    /**
     * Parses date corrections.
     *
     * @param changeSection
     *         change html
     *
     * @return date corrections
     */
    static List<ParsedCorrigendum> parseDateCorrections(final Element changeSection) {
        final List<ParsedCorrigendum> dateCorrections = new ArrayList<>();

        final Elements dateChangesHtmls = getDateChangesHtmls(changeSection);
        for (Element dateChangeHtml : dateChangesHtmls) {
            final String sectionNumber = parseCorrectionSectionNumber(dateChangeHtml);
            if (StringUtils.isNotEmpty(sectionNumber)) {
                final ParsedCorrigendum corrigendum = new ParsedCorrigendum().setSectionNumber(sectionNumber)
                        .setLotNumber(parseCorrectionLotNumber(dateChangeHtml))
                        .setPlaceOfModifiedText(parseCorrectionPlace(dateChangeHtml))
                        .setOriginalDate(parseCorrectionOriginalDate(dateChangeHtml))
                        .setReplacementDate(parseCorrectionReplacementDate(dateChangeHtml));
                dateCorrections.add(corrigendum);
            }
        }
        return dateCorrections;
    }

    /**
     * Gets text changes htmls.
     *
     * @param changeSection
     *         change html
     *
     * @return text changes htmls
     */
    private static Elements getTextChangesHtmls(final Element changeSection) {
        return changeSection.select("div[model~=.*\\.ChangeTextList\\[\\d+\\]$]");
    }

    /**
     * Get CPV changes htmls.
     *
     * @param changeSection
     *         change html
     *
     * @return CPV changes htmls
     */
    static Elements getCpvChangesHtmls(final Element changeSection) {
        return changeSection.select("div[model~=.*\\.ChangeCpvList\\[\\d+\\]$]");
    }

    /**
     * Get date chagnes htmls.
     *
     * @param changeSection
     *         change html
     *
     * @return date changes htmls
     */
    private static Elements getDateChangesHtmls(final Element changeSection) {
        return changeSection.select("div[model~=.*\\.ChangeDateList\\[\\d+\\]$]");
    }

    /**
     * Parses correction section number.
     *
     * @param changeSection
     *         change html
     *
     * @return correction section number
     */
    static String parseCorrectionSectionNumber(final Element changeSection) {
        return VVZTenderParserUtils.getFieldValue(changeSection, ".*\\.Where\\.Section$");
    }

    /**
     * Parses correction lot number.
     *
     * @param changeSection
     *         change html
     *
     * @return correction lot number
     */
    static String parseCorrectionLotNumber(final Element changeSection) {
        return VVZTenderParserUtils.getFieldValue(changeSection, ".*\\.Where\\.LotNumber$");
    }

    /**
     * Parses correction place.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction place
     */
    static String parseCorrectionPlace(final Element changeHtml) {
        return VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.Where\\.Label$");
    }

    /**
     * Parses correction original text.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction original text
     */
    private static String parseCorrectionOriginalText(final Element changeHtml) {
        return VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.OldText$");
    }

    /**
     * Parses correction replacement text.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction replacement text
     */
    private static String parseCorrectionReplacementText(final Element changeHtml) {
        return VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.NewText$");
    }

    /**
     * Parses correction original date.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction original date
     */
    private static String parseCorrectionOriginalDate(final Element changeHtml) {
        final String originalDate = VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.OldDate$");
        final String originalTime = VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.OldTime$");
        return VVZTenderParserUtils.concatenateStrings(originalDate, originalTime);
    }

    /**
     * Parses correction replacement date.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction replacement date
     */
    private static String parseCorrectionReplacementDate(final Element changeHtml) {
        final String replacementDate = VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.NewDate$");
        final String replacementTime = VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.NewTime$");
        return VVZTenderParserUtils.concatenateStrings(replacementDate, replacementTime);
    }

    // ---------------------------------
    // END OF SUBSECTION
    // ---------------------------------

    /**
     * Parses whether form is EU form.
     *
     * @param formCode
     *         form code
     *
     * @return true if form is EU form
     */
    private static boolean isEuropeanForm(final String formCode) {
        return formCode.startsWith("F");
    }

    /**
     * Parses info about related publication in Vestnik, that is all forms that
     * has been published for this tender in Vestnik. These related forms are
     * found by using appropriate search functionality in Vestnik and the result
     * of this search is provided by crawler/downloader within the metadata in
     * the {@code rawObject}.
     *
     * @param relatedPublicationsForm
     *         parsed document tree for the list of related publications page
     * @param originalFormId
     *         source id of the original (included) form
     *
     * @return list of publications in Vestnik related to the same tender or
     * empty list if no such forms found
     */
    private static List<ParsedPublication> parseRelatedVestnikPublications(final Document relatedPublicationsForm,
            final String originalFormId) {
        final List<ParsedPublication> publications = new ArrayList<>();

        final List<Element> relatedResultRows = relatedPublicationsForm.select(
                "div#SearchFormGrid > table > tbody > tr");

        for (final Element relatedResultRow : relatedResultRows) {
            final Elements relatedResultData = relatedResultRow.select("td");

            final String publicationSourceId = relatedResultData.get(0).text();

            // skip this publication
            if (!publicationSourceId.equals(originalFormId)) {
                publications.add(new ParsedPublication().setIsIncluded(false)
                        .setSource(VESTNIK_SOURCE_URL)
                        .setSourceFormType(relatedResultData.get(4).ownText())
                        .setSourceId(publicationSourceId)
                        .setPublicationDate(relatedResultData.get(5).text())
                        .setHumanReadableUrl(relatedResultData.get(0).select("a").first().attr("abs:href")));
            }
        }
        return publications;
    }

    /**
     * Parses body details from given form section with body information.
     *
     * @param bodySection
     *         HTML fragment for section containing information about body
     *
     * @return parsed information about body
     */
    static ParsedBody parseBody(final Element bodySection) {
        if (bodySection == null) {
            logger.warn("Unable to parse body information. Provided HTML element is null.");
            return null;
        }

        ParsedBody body = new ParsedBody();
        String ico = getFieldValue(bodySection, ".*\\.NationalId");

        if (StringUtils.isNotEmpty(ico)) {
            body.addBodyId(new BodyIdentifier().setId(ico)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                    .setScope(BodyIdentifier.Scope.CZ));
        }

        body.setName(getFieldValue(bodySection, ".*\\.OfficalName"))
                .setEmail(getFieldValue(bodySection, ".*\\.EMail"))
                .setPhone(getFieldValue(bodySection, ".*\\.Phone"))
                .setContactName(getFieldValue(bodySection, ".*\\.ContactPoint"))
                //.setBuyerProfileUrl(getFieldValue(bodySection, ".*\\.UrlBuyer"))
                .setAddress(new ParsedAddress().setStreet(getFieldValue(bodySection, ".*\\.Address$"))
                        .setCity(getFieldValue(bodySection, ".*\\.Town"))
                        .addNuts(getFieldValue(bodySection, ".*\\.Nuts"))
                        .setPostcode(getFieldValue(bodySection, ".*\\.Postal"))
                        .setCountry(getSelectedOptionValue(bodySection, ".*\\.Country"))
                        .setUrl(getFieldValue(bodySection, ".*\\.UrlGeneral")));

        return body;
    }

    /**
     * Parses CPVs.
     *
     * @param sectionWithCpvs
     *         html with CPVs
     *
     * @return CPVs
     */
    static List<ParsedCPV> parseCPVCodes(final Element sectionWithCpvs) {
        final List<ParsedCPV> cpvs = new ArrayList<>();
        // main CPV
        final String mainCpvCode = parseMainCPVCode(sectionWithCpvs);
        if (StringUtils.isNotEmpty(mainCpvCode)) {
            cpvs.add(new ParsedCPV().setIsMain(Boolean.TRUE.toString()).setCode(mainCpvCode));
        }

        // other CPVs
        cpvs.addAll(sectionWithCpvs.select("input[name~=.*\\.CpvSupplementary\\[\\d+\\]$]")
                .stream()
                .filter(cpvDetail -> !cpvDetail.attr("value").isEmpty())
                .map(cpvDetail -> new ParsedCPV().setIsMain(Boolean.FALSE.toString()).setCode(cpvDetail.attr("value")))
                .collect(Collectors.toList()));
        return cpvs;
    }

    /**
     * Parses main CPV code.
     *
     * @param sectionWithCpvs
     *         html with CPV info
     *
     * @return main CPV code
     */
    private static String parseMainCPVCode(final Element sectionWithCpvs) {
        return VVZTenderParserUtils.getFieldValue(sectionWithCpvs, ".*\\.CpvMain$");
    }

    /**
     * Parses related original publication from form header.
     *
     * @param form
     *         form html
     *
     * @return original publication
     */
    static ParsedPublication parseRelatedOriginalPublicationFromHeader(final Document form) {
        return new ParsedPublication().setIsIncluded(false)
                .setSource(VVZTenderParser.VESTNIK_SOURCE_URL)
                .setSourceId(parseRelatedPublicationFormId(form))
                .setSourceTenderId(VVZTenderParser.parseTenderId(form));
    }

    /**
     * Parses related publication form id.
     *
     * @param form
     *         form html
     *
     * @return related publication form id
     */
    static String parseRelatedPublicationFormId(final Document form) {
        return VVZTenderParserUtils.parseInfoFromHeader(form, "Evidenční číslo souvisejícího formuláře");
    }

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "CZ";
    }

    @Override
    protected List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}