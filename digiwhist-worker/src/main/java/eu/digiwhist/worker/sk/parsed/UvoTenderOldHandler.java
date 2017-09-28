package eu.digiwhist.worker.sk.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.cz.parsed.VestnikTenderParserUtils;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getFirstValueWithoutDots;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getFormType;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getTrueOrFalseFromElement;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getValuesFromElement;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

/**
 * Tender old form parsed for Slovakia.
 *
 * @author Michal Riha
 */
class UvoTenderOldHandler {
    private static final Logger logger = LoggerFactory.getLogger(VestnikTenderParserUtils.class);

    private static final String IN_PART_I = "legend:matchesOwn(ODDIEL I[\\.:].*) + div ";
    private static final String IN_PART_II = "legend:matchesOwn(ODDIEL II.*) + div ";
    private static final String IN_PART_III = "legend:matchesOwn(ODDIEL III.*) + div ";
    private static final String IN_PART_IV = "legend:matchesOwn(ODDIEL IV.*) + div ";
    private static final String IN_PART_VI = "legend:matchesOwn(ODDIEL VI.*) + div ";

    private static final String SOURCE_DOMAIN = "https://www.uvo.gov.sk";

    private static final int ORIGINAL_VALUE_BEHIND = 3;
    private static final int CORRECTION_VALUE_BEHIND = 4;

    /**
     * Parse common data for all ancient forms.
     *
     * @param document        document to parse data from
     * @param url             url of the document
     * @param publicationDate publication date of the document
     *
     * @return List<ParsedTender> with parsed data
     */
    List<ParsedTender> parse(final Document document, final String url, final String publicationDate) {

        // if publication is correction, don't parse common attributes
        if (url.contains("correction")) {
            final Elements corrections = JsoupUtils.select("div[id=corrections] tr > td", document);
            final ParsedTender parsedTender = new ParsedTender();

            for (Element correction : corrections) {
                final List<TextNode> textNodes = correction.textNodes();

                if (textNodes != null && !textNodes.isEmpty()) {
                    final List<String> strings = textNodes.stream().map(TextNode::text).collect(Collectors.toList());

                    String lastKnownSection = null;
                    for (int position = 0; position < strings.size();) {

                        // correction data are next and there are original + replacement data, or section data
                        if (strings.get(position).contains(
                                "iesto") && strings.size() > position + CORRECTION_VALUE_BEHIND) {

                            // date correction or other value
                            if (strings.get(position + ORIGINAL_VALUE_BEHIND).trim().matches(
                                    "^(0[1-9]|[12][0-9]|3[01])\\.(0[1-9]|[12][0-9]|3[01])\\.(19|20)\\d*.*")) {
                                parsedTender.addCorrigendum(new ParsedCorrigendum()
                                        .setSectionNumber(lastKnownSection)
                                        .setOriginalDate(strings.get(position + ORIGINAL_VALUE_BEHIND))
                                        .setReplacementDate(strings.get(position + CORRECTION_VALUE_BEHIND)));
                            } else {
                                parsedTender.addCorrigendum(new ParsedCorrigendum()
                                        .setSectionNumber(lastKnownSection)
                                        .setOriginal(strings.get(position + ORIGINAL_VALUE_BEHIND))
                                        .setReplacement(strings.get(position + CORRECTION_VALUE_BEHIND)));
                            }

                            position = position + CORRECTION_VALUE_BEHIND;
                        } else if (strings.get(position).trim().matches("^(IX|IV|V?I{0,3})\\..*")) {
                            lastKnownSection = strings.get(position);
                            position++;
                        } else {
                            position++;
                        }
                    }
                }
            }

            return Collections.singletonList(parsedTender);
        }

        // parse common attributes
        ParsedTender parsedTender = new ParsedTender()
                .addPublication(new ParsedPublication()
                        .setSource(PublicationSources.SK_UVO)
                        .setPublicationDate(publicationDate)
                        .setSourceId(parsePublicationSourceId(document))
                        .setSourceFormType(parsePublicationSourceFormType(document))
                        .setDispatchDate(parsePublicationDispatchDate(document))
                        .setBuyerAssignedId(parseBuyerAssignedId(document))
                        .setHumanReadableUrl(url)
                        .setIsIncluded(true)
                        .setLanguage("SK"))
                .addPublications(parsePreviousPublication(document))
                .setNationalProcedureType(parseTenderNationalProcedureType(document))
                .setSupplyType(parseTenderSupplyType(document))
                .setPersonalRequirements(parsePersonalRequirements(document))
                .setEconomicRequirements(parseEconomicRequirements(document))
                .setTechnicalRequirements(parseTechnicalRequirements(document))
                .setSelectionMethod(parseTenderSelectionMethod(document))
                .setIsJointProcurement(parseIsJointProcurement(document))
                .setIsCentralProcurement(parseIsCentralProcurement(document))
                .setIsFrameworkAgreement(parseIsFrameworkAgreement(document))
                .addBuyer(new ParsedBody().setName(parseBuyerName(document))
                        .addBodyId(new BodyIdentifier()
                                .setId(parseBuyerBodyIdentifierId(document))
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                .setScope(BodyIdentifier.Scope.SK))
                        .setAddress(new ParsedAddress()
                                .setRawAddress(parseBuyerRawAddress(document))
                                .setCountry(parseBuyerCountry(document))
                                .setUrl(parseBuyerWebAddress(document)))
                        .setContactPoint(parseBuyerContactPoint(document))
                        .setContactName(parseBuyerContactName(document))
                        .setPhone(parseBuyerPhone(document))
                        .setEmail(parseBuyerEmail(document))
                        .setBuyerType(parseBuyerType(document))
                        .setMainActivities(parseBuyerMainActivities(document)))
                .setIsOnBehalfOf(parseIsTenderOnBehalfOfSomeone(document))
                .setTitle(parseTenderTitle(document))
                .setAddressOfImplementation(
                        new ParsedAddress()
                                .setRawAddress(parseRawAddressOfImplementation(document))
                                .addNuts(parseNutsOfImplementation(document)))
                .setDescription(parseTenderDescription(document))
                .setCpvs(parseTenderNotMainCpvs(document))
                .addCpv(parseTenderMainCpv(document))
                .setIsCoveredByGpa(parseIsTenderCoveredByGpa(document))
                .setHasLots(parseIfTenderHasLots(document))
                .setAreVariantsAccepted(parseAreVariantsAccepted(document))
                .setEstimatedPrice(parseEstimatedPrice(document))
                .setHasOptions(parseIfTenderHasOptions(document))
                .setEstimatedDurationInMonths(parseTenderEstimatedDurationInMonths(document))
                .setDeposits(parseTenderDeposits(document))
                .setAwardCriteria(parseAwardCriteria(document))
                .setIsElectronicAuction(parseIfTenderIsElectronicAuction(document))
                .setDocumentsDeadline(parseDocumentsDeadline(document))
                .setDocumentsPayable(parseIfTenderDocumentsPayable(document))
                .setDocumentsPrice(parseDocumentsPrice(document))
                .setBidDeadline(parseTenderBidDeadline(document))
                .setAwardDeadline(parseAwardDeadline(document))
                .setEligibleBidLanguages(parseTenderEligibleBidLanguages(document))
                .setAppealBodyName(parseTenderAppealBodyName(document))
                .addFunding(new ParsedFunding()
                        .setProgramme(parseFundingProgramme(document))
                        .setIsEuFund(parseIsEUFunded(document)))
                .addPublications(parseRelatedPublications(document))
                .setMediationBodyName(parseMediationBodyName(document));

        //         parse form specific attributes
        PublicationFormType formType = getFormType(parsedTender.getPublications().get(0).getSourceFormType());
        switch (formType) {
            case CONTRACT_NOTICE:
                parsedTender = UvoTenderOldOzHandler.parse(parsedTender, document);
                break;
            case CONTRACT_AWARD:
                parsedTender = UvoTenderOldOzzHandler.parse(parsedTender, document);
                break;
            case CONTRACT_CANCELLATION:
                parsedTender = UvoTenderOldZzzHandler.parse(parsedTender, document);
                break;
            default:
                logger.warn("Unknown publication form type.");
        }

        return new ArrayList<>(Arrays.asList(parsedTender));
    }

    /**
     * Parse funding programme.
     *
     * @param document document to parse from
     *
     * @return String or null
     */
    private String parseFundingProgramme(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_VI + "span:containsOwn(dkaz na projekt (projekty) a/alebo program (programy)) + span");
    }

    /**
     * Parse if tender is joint procurement.
     *
     * @param document document to parse
     *
     * @return string or null
     */
    private String parseIsJointProcurement(final Document document) {
        return getTrueOrFalseFromElement(document,
                IN_PART_I + "div:containsOwn(Zákazka zahŕňa spoločné obstarávanie) > span");
    }

    /**
     * Parse if tender is central procurement.
     *
     * @param document document to parse
     *
     * @return string or null
     */
    private String parseIsCentralProcurement(final Document document) {
        return getTrueOrFalseFromElement(document,
                IN_PART_I + "div:containsOwn(Zákazku prideľuje ústredný obstarávací orgán) > span");
    }

    /**
     * Parse if there is framework agreement.
     *
     * @param document document to be parsed
     *
     * @return string or null
     */
    private String parseIsFrameworkAgreement(final Document document) {
        String frameworkAgreement = getFirstValueFromElement(document, new String[]{
                IN_PART_II + "div:has(span:containsOwn(Informácie o verejnej zákazke)) + div",
                IN_PART_II + "div:has(span:containsOwn(nformácie o rámcovej dohode)) + div"});

        if (frameworkAgreement == null) {
            return null;
        } else {
            return String.valueOf(frameworkAgreement.toLowerCase()
                    .contains("uzavretie rámcovej dohody".toLowerCase()));
        }
    }

    /**
     * Parse mediation body name.
     *
     * @param document document to parse from
     *
     * @return return parsed mediation body name
     */
    private String parseMediationBodyName(final Document document) {
        return getFirstValueFromElement(document, IN_PART_VI + "div:has(span:containsOwn(zodpovedný za mediáci)) + " +
                "div span > span");
    }

    /**
     * Parse related publications.
     *
     * @param document document to parse from
     *
     * @return return parsed publications
     */
    private List<ParsedPublication> parseRelatedPublications(final Document document) {
        final List<Element> relatedRawPublications = document.select("a:has(h2:containsOwn(Súvisiace oznámenia)) + " +
                "div tbody > tr");

        if (relatedRawPublications == null) {
            return null;
        }

        final List<ParsedPublication> relatedPublications = new ArrayList<>();

        for (Element relatedRawPublication : relatedRawPublications) {
            List<String> relatedPublicationValues = getValuesFromElement(relatedRawPublication, "td");

            String relatedPublicationUrl = relatedRawPublication.attr("onclick");
            if (relatedPublicationUrl != null) {
                relatedPublicationUrl =
                        SOURCE_DOMAIN + relatedPublicationUrl.replace("window.location.href = '", "").replace("'", "");
            }

            if (relatedPublicationValues != null && !relatedPublicationValues.isEmpty()) {
                String[] publicationSourceIdAndFormType = relatedPublicationValues.get(0).split(" - ");

                if (publicationSourceIdAndFormType.length > 1) {
                    relatedPublications.add(new ParsedPublication()
                            .setSource(PublicationSources.SK_UVO)
                            .setSourceId(publicationSourceIdAndFormType[0])
                            .setSourceFormType(publicationSourceIdAndFormType[1])
                            .setHumanReadableUrl(relatedPublicationUrl)
                            .setIsIncluded(false));
                }
            }
        }

        return relatedPublications;
    }

    /**
     * Parse if tender is EU funded.
     *
     * @param document document to parse from
     *
     * @return return EU funded value
     */
    private String parseIsEUFunded(final Document document) {
        return getTrueOrFalseFromElement(document, new String[]{
                IN_PART_VI + "div:containsOwn(financovaného z fondov Európskej únie) > span",
                IN_PART_VI + "div:containsOwn(yplňte) > span"});
    }

    /**
     * Parse award deadline.
     *
     * @param document document to parse from
     *
     * @return return award deadline
     */
    private String parseAwardDeadline(final Document document) {
        final String awarddeadline = getFirstValueFromElement(document, IN_PART_IV + "div:has(span:containsOwn" +
                "(inimálna lehota, počas ktorej sú ponuky uchádzačov viaz)) + div + div:containsOwn(Dátum:)");

        if (awarddeadline != null) {
            return awarddeadline.replace("Dátum:", "").replace("Čas:", "");
        } else {
            return null;
        }
    }

    /**
     * Parse previous publications.
     *
     * @param document document to parse from
     *
     * @return return parsed previous publications
     */
    private List<ParsedPublication> parsePreviousPublication(final Document document) {
        final List<String> previousPublicationSourceIds = getValuesFromElement(document, IN_PART_IV +
                "div:containsOwn" +
                "(Číslo oznámenia v Ú. v. EÚ) span");
        final List<String> previousPublicationDates = getValuesFromElement(document, IN_PART_IV + "div:containsOwn" +
                "(Číslo " +
                "oznámenia v Ú. v. EÚ) + div:containsOwn(z:)");

        final List<ParsedPublication> result = new ArrayList<>();

        if (previousPublicationSourceIds != null && !previousPublicationSourceIds.isEmpty()) {
            for (int i = 0; i < previousPublicationSourceIds.size(); i++) {
                final String previousPublicationSourceId = previousPublicationSourceIds.get(i);
                String previousPublicationDate = null;
                if (previousPublicationDates != null && previousPublicationDates.size() > i) {
                    previousPublicationDate = previousPublicationDates.get(i);
                }

                result.add(new ParsedPublication()
                        .setSource(PublicationSources.EU_TED)
                        .setSourceId(previousPublicationSourceId)
                        .setPublicationDate(previousPublicationDate)
                        .setIsIncluded(false));
            }

            return result;
        } else {
            return null;
        }
    }

    /**
     * Parse technical requirements.
     *
     * @param document document to parse from
     *
     * @return return technical requirements
     */
    private String parseTechnicalRequirements(final Document document) {
        return getFirstValueFromElement(document, IN_PART_III + "div:has(span:containsOwn(Technická spôsobilosť)) + " +
                "div + div:not(:contains(III.))");
    }

    /**
     * Parse economic requirements.
     *
     * @param document document to parse from
     *
     * @return return economic requirements
     */
    private String parseEconomicRequirements(final Document document) {
        return getFirstValueFromElement(document, IN_PART_III + "div:has(span:containsOwn(onomická a finančná " +
                "spôsobilosť)) + div + div:not(:contains(III.))");
    }

    /**
     * Parse personal requirements.
     *
     * @param document document to parse from
     *
     * @return return personal requirements
     */
    private String parsePersonalRequirements(final Document document) {
        return getFirstValueFromElement(document, IN_PART_III + "div:has(span:containsOwn(bné postavenie " +
                "hospodárskych subjektov vrátane požia)) + div + div:not(:contains(III.))");
    }

    /**
     * Parse publication source ID value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parsePublicationSourceId(final Document document) {
        String[] parsedPublicationSourceInfo = parsePublicationSourceInfo(document);

        if (parsedPublicationSourceInfo != null && parsedPublicationSourceInfo.length >= 1) {
            return parsedPublicationSourceInfo[0].trim();
        }

        return null;
    }

    /**
     * Parse form type of publication source from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parsePublicationSourceFormType(final Document document) {
        String[] parsedPublicationSourceInfo = parsePublicationSourceInfo(document);

        if (parsedPublicationSourceInfo != null && parsedPublicationSourceInfo.length >= 2) {
            return parsedPublicationSourceInfo[1].trim();
        }

        return null;
    }

    /**
     * Parse publication dispatch date value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parsePublicationDispatchDate(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_VI + "div.subtitle:has(span.title:containsOwn(Dátum odoslania)) + div");
    }

    /**
     * Parse NUTS code of implementation value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseNutsOfImplementation(final Document document) {
        return getFirstValueWithoutDots(document, new String[]{
                IN_PART_II + "span:containsOwn(NUTS) + span",
                IN_PART_II + "span:containsOwn(NUTS) ~ div",
                IN_PART_II + "div:has(span:containsOwn(NUTS)) + div span:containsOwn(SK)",
                IN_PART_II + "div:has(span:containsOwn(NUTS)) + div div:containsOwn(SK)"});
    }

    /**
     * Parse  value from document.
     *
     * @param document document to be parsed
     *
     * @return List<ParsedCpv> or Null
     */
    private List<ParsedCPV> parseTenderNotMainCpvs(final Document document) {
        List<ParsedCPV> parsedCPVs = new ArrayList<>();
        String cpvCodesString = getFirstValueFromElement(document,
                IN_PART_II + "div:has(span:containsOwn(CPV)) + div > div:containsOwn(Doplňujúce predmety) + div");

        if (cpvCodesString == null) {
            return null;
        }

        String[] cpvCodes = cpvCodesString.replace("Hlavný slovník:", "").replace("Doplňujúce predmety", "").split(",");

        for (String cpvCode : cpvCodes) {
            final String code = cpvCode.trim().replaceAll("\\.+$", "").trim();

            if (!code.isEmpty()) {
                parsedCPVs.add(new ParsedCPV()
                        .setCode(code)
                        .setIsMain(String.valueOf(false)));
            }
        }

        return parsedCPVs.isEmpty() ? null : parsedCPVs;
    }

    /**
     * Parse tender main CPV value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private ParsedCPV parseTenderMainCpv(final Document document) {
        String mainCpv = getFirstValueFromElement(document,
                IN_PART_II + "div:has(span:containsOwn(CPV)) + div > div > span:containsOwn(Hlavný slov)");

        if (mainCpv == null) {
            return null;
        }

        mainCpv = mainCpv.replace("Hlavný slovník:", "").trim().replaceAll("\\.+$", "").trim();

        return mainCpv.isEmpty() ? null : new ParsedCPV()
                .setCode(mainCpv)
                .setIsMain(String.valueOf(true));
    }

    /**
     * Parse tender appeal body name value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderAppealBodyName(final Document document) {
        Element element = document.select(
                IN_PART_VI + "div:has(span:containsOwn(za odvolacie konanie))" + " + div span").first();

        if (element == null) {
            return null;
        }

        return element.html().split("<br>")[0].replaceAll("\\<[^)]*\\>", "");
    }

    /**
     * Parse tender eligible bid languages value from document.
     *
     * @param document document to be parsed
     *
     * @return String[] or Null
     */
    private List<String> parseTenderEligibleBidLanguages(final Document document) {
        List<String> eligibleBidLanguages = getValuesFromElement(document,
                IN_PART_IV + "div:has(span:containsOwn(Úradný jazyk/úradné jazyky EÚ)) + div > div");

        if (eligibleBidLanguages != null) {
            return eligibleBidLanguages;
        }

        String tempEligibleBidLanguages = getFirstValueFromElement(document,
                IN_PART_IV + "div:has(span:containsOwn(Jazyk (jazyky))) + div + div > span");

        if (tempEligibleBidLanguages == null) {
            return null;
        }

        return Arrays.asList(tempEligibleBidLanguages.split(","));
    }

    /**
     * Parse tender enquiry deadline value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderBidDeadline(final Document document) {
        String enquiryDeadling = getFirstValueFromElement(document,
                IN_PART_IV + "div:has(span:containsOwn(Lehota na predkladanie ponúk)) + div" +
                        ":not(:contains(Leta)):not(:contains(IV))");

        if (enquiryDeadling == null) {
            return null;
        }

        return enquiryDeadling.replace("Dátum a čas:", "");
    }

    /**
     * Parse if tender documents are payable value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderDocumentsPayable(final Document document) {
        return getTrueOrFalseFromElement(document,
                new String[]{IN_PART_IV + "div:containsOwn(Spoplatnená dokumentácia) > span", IN_PART_IV + "div:has"
                        + "(span:containsOwn(Úhrada za súťažné podklady)) + div > span"});
    }

    /**
     * Parse tender documents price.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private ParsedPrice parseDocumentsPrice(final Document document) {
        return parsePrice(document, false,
                IN_PART_IV + "div:containsOwn(Spoplatnená dokumentácia) + div:containsOwn(Cena) > span",
                IN_PART_IV + "div:containsOwn(Spoplatnená dokumentácia) + div:containsOwn(Cena) > span + span", null);
    }

    /**
     * Parse bid deadline value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseDocumentsDeadline(final Document document) {
        String bidDeadline = getFirstValueFromElement(document,
                IN_PART_IV + "div:has(span:containsOwn(Lehota na prijímanie žiadostí o súťažné podklady)) + " +
                        "div:not(:contains(IV))");

        if (bidDeadline == null) {
            return null;
        }

        return bidDeadline.replace("Dátum:", "").replace("Dátum a čas:", "");
    }

    /**
     * Parse if tender is electronic auction value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderIsElectronicAuction(final Document document) {
        return getTrueOrFalseFromElement(document, new String[]{
                IN_PART_IV + "div:containsOwn(sa elektronická aukcia) > span",
                IN_PART_IV + "div:has(span:containsOwn(sa elektronická aukcia)) + div > span"});
    }

    /**
     * Parse tender deposits value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderDeposits(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_III + "div:has(span:containsOwn(Požadované zábezpeky a záruky)) + div");
    }

    /**
     * Parse tender estimated duration in months value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderEstimatedDurationInMonths(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_II + "div:has(span:containsOwn(TRVANIE Z)) + div:has(span:containsOwn(mesiacoch)) + " +
                        "div span");
    }

    /**
     * Parse if tender has options value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderHasOptions(final Document document) {
        return getTrueOrFalseFromElement(document,
                IN_PART_II + "div:has(span:containsOwn(Informácie o opciách)) + div > span");
    }

    /**
     * Parse tender estimated price.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private ParsedPrice parseEstimatedPrice(final Document document) {
        String[] priceSelectors = new String[]{
                IN_PART_II + "div:has(span:containsOwn(predpokladaná hodnota)) + div:containsOwn(Hodnota):not(:" +
                        "matches(Od)) > span",
                IN_PART_II + "div:has(span:containsOwn(množstvo alebo rozsah)) + div + div:containsOwn(hodnota) > span",
                IN_PART_II + "div:has(span:containsOwn(množstvo alebo rozsah)) + div:containsOwn(hodnota) > span",
                IN_PART_II + "div:containsOwn(Hodnota):not(:matches(Od)) > span"};

        String[] currencySelectors = new String[]{
                IN_PART_II + "div:has(span:containsOwn(predpokladaná hodnota)) + div:containsOwn(Hodnota):not(:" +
                        "matches(Od)) > span + span",
                IN_PART_II + "div:has(span:containsOwn(množstvo alebo rozsah)) + div + div:containsOwn(hodnota) > " +
                        "span + span",
                IN_PART_II + "div:has(span:containsOwn(množstvo alebo rozsah)) + div:containsOwn(hodnota) > span + " +
                        "span",
                IN_PART_II + "div:containsOwn(Hodnota):not(:matches(Od)) > span + span"};


        ParsedPrice parsedPrice = parsePrice(document, false, priceSelectors, currencySelectors, null);

        if (parsedPrice == null) {
            final String minPrice = getFirstValueFromElement(document,
                    IN_PART_II + "div:containsOwn(Hodnota):matches(Od) > span");
            final String maxPrice = getFirstValueFromElement(document,
                    IN_PART_II + "div:containsOwn(Hodnota):matches(Od) > span + span");

            if (minPrice != null && maxPrice != null) {
                parsedPrice = new ParsedPrice()
                        .setMinNetAmount(minPrice)
                        .setMaxNetAmount(maxPrice)
                        .setCurrency(getFirstValueFromElement(document,
                                IN_PART_II + "div:containsOwn(Hodnota):matches(Od) > span + span + span"));
            }
        }

        return parsedPrice;
    }

    /**
     * Parse if variants are accepted value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseAreVariantsAccepted(final Document document) {
        return getTrueOrFalseFromElement(document,
                new String[]{IN_PART_II + "div:containsOwn(Varianty sa budú prijímať) > span", IN_PART_II + "div:has"
                        + "(span:containsOwn(predloženie variantných riešení)) + div > span"});
    }

    /**
     * Parse if tender has lots value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderHasLots(final Document document) {
        return getTrueOrFalseFromElement(document, new String[]{
                IN_PART_II + "div:containsOwn(zákazka sa delí na čast) > span",
                IN_PART_II + "div:has(span:containsOwn(Rozdelenie na časti)) + div > span"});
    }

    /**
     * Parse tender national procedure type value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderNationalProcedureType(final Document document) {
        String nationalProcedureType = getFirstValueFromElement(document, new String[]{
                IN_PART_IV + "div:has(span:containsOwn(druh)) + div:not(:contains(IV))",
                "div.notice > div:has(strong:containsOwn(Druh postupu:):not(:containsOwn(Osoby))"});

        if (nationalProcedureType == null) {
            return null;
        }

        return nationalProcedureType.replace("Dátum:", "").replace("Druh postupu", "").replaceAll("\\.+$", "")
                .replaceAll(":", "");
    }

    /**
     * Parse tender selection method from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderSelectionMethod(final Document document) {
        return getFirstValueFromElement(document, IN_PART_IV + "span:matchesOwn((nomicky najvýhodnejšia ponuka z " +
                "hľadiska|ajnižšia cena)):not(:matchesOwn(-|%|ritérium)):not(:matchesOwn(^\\d))");
    }

    /**
     * Parse tender supply type value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderSupplyType(final Document document) {
        String supplyType = getFirstValueFromElement(document, new String[]{
                "div.notice > div:has(strong:containsOwn(Druh zákazky:)",
                "div.notice div:containsOwn(Druh zákazky) > span"});

        if (supplyType == null) {
            return null;
        }

        return supplyType.replace("Druh zákazky:", "");
    }

    /**
     * Parse tender title value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderTitle(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_II + "div:has(span:containsOwn(Názov pridelený zákazke)) + div",
                IN_PART_II + "div:has(span:matchesOwn(Názov (zákazky|súťaže))) + div"});
    }

    /**
     * Parse tender description value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderDescription(final Document document) {
        return getFirstValueFromElement(document, IN_PART_II + "div:has(span:containsOwn(Stručný opis)) + div");
    }

    /**
     * Parse buyer assigned ID value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerAssignedId(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_IV + "div:has(span:matchesOwn((Referečné|Referenčné|Evidenčné) číslo spisu)) + div");
    }

    /**
     * Parse if tender is covered by GPA.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIsTenderCoveredByGpa(final Document document) {
        return getTrueOrFalseFromElement(document, new String[]{
                IN_PART_II + "div:containsOwn((GPA)) > span",
                IN_PART_II + "div:contains(vzťahuje dohoda o vládnom obstarávaní) + div > span"});
    }

    /**
     * Parse buyer name value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerName(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "span.titleValue > span");
    }

    /**
     * Parse buyer contact point value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerContactPoint(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "span:containsOwn(Kontaktné miesto) + span");
    }

    /**
     * Parse buyer contact name value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerContactName(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "span:containsOwn(Kontaktná osoba) + span");
    }

    /**
     * Parse buyer phone value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerPhone(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "span:containsOwn(Telefón) + span");
    }

    /**
     * Parse buyer email value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerEmail(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "span:containsOwn(Email) + span");
    }

    /**
     * Parse buyer web address value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerWebAddress(final Document document) {
        return getFirstValueFromElement(document,
                new String[]{IN_PART_I + "span:containsOwn((URL)) + span", IN_PART_I + "span:containsOwn(Internetová "
                        + "adresa) + span"});
    }

    /**
     * Parse buyer type value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerType(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_I + "div:has(span:containsOwn(Druh verejného obstarávateľa):not(:containsOwn(Hlav))) + div > " +
                        "span:eq(1):not(:containsOwn(Iné)):not(:containsOwn(Druh verejného obstarávat))",
                IN_PART_I + "div:containsOwn(Iný verejný obstarávateľ) > span"});
    }

    /**
     * Parse buyer main activity value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private List<String> parseBuyerMainActivities(final Document document) {
        final Element firstLine = document.select(IN_PART_I + "div:has(span.title:containsOwn(HLAVN):not(:containsOwn" +
                "(druh)))").first();
        final Element lastLine = document.select(IN_PART_I + "div:has(span.title:containsOwn(HLAVN):not(:containsOwn" +
                "(druh))) ~ div:has(span:containsOwn(I.))").first();
        final Element root = ParserUtils.getSubsectionOfElements(firstLine, lastLine);

        final List<String> results = new ArrayList<>();
        final List<String> mainActivities = getValuesFromElement(root, "div > div");

        if (mainActivities == null) {
            return null;
        } else {
            for (String result : mainActivities) {
                if (!result.replace("Iné (uveďte)", "").replace("Iný predmet", "").replace("(špecifikujte)", "")
                        .replaceAll(":", "")
                        .trim()
                        .isEmpty()) {
                    results.add(result);
                }
            }
        }

        final List<String> otherActivities = getValuesFromElement(root, "div:containsOwn(Iný) > span");

        if (otherActivities == null) {
            return null;
        } else {
            for (String result : otherActivities) {
                if (!result.replace("Iné (uveďte)", "").replace("Iný predmet", "").replace("(špecifikujte)", "")
                        .replaceAll(":", "")
                        .trim()
                        .isEmpty()) {
                    results.add("Iný predmet: " + result);
                }
            }
        }

        return results.isEmpty() ? null : results;
    }

    /**
     * Parse buyer body identifier ID value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerBodyIdentifierId(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "span.titleValue span:eq(3)");
    }

    /**
     * Parse buyer raw address value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerRawAddress(final Document document) {
        Element rawAddressElement = document.select(IN_PART_I + "span.titleValue").first();

        if (rawAddressElement == null) {
            return null;
        }

        String[] rawAddressArray = rawAddressElement.html().split("<br>");

        if (rawAddressArray.length < 3) {
            return null;
        }

        return rawAddressArray[2];
    }

    /**
     * Parse buyer country value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerCountry(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "div.ContactSelectList > span:eq(2)");
    }

    /**
     * Parse if tender is made on behalf of someone value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIsTenderOnBehalfOfSomeone(final Document document) {
        return getTrueOrFalseFromElement(document,
                new String[]{IN_PART_I + "div:has(span:containsOwn(V MENE INÝCH VEREJN)) + div" + " > span:not"
                        + "(:containsOwn(EREJ))", IN_PART_I + "div:has(span:containsOwn(pre iných verejn)) + div" + ""
                        + " > span:not(:containsOwn(EREJ))"});
    }

    /**
     * Parse raw address of implementation value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseRawAddressOfImplementation(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_II + "span:matchesOwn((miesto|lokalita) (poskytovania|uskutočňovania))" + ":not(:containsOwn"
                        + "(Druh)) + span",
                IN_PART_II + "div:matchesOwn((miesto|lokalita) (poskytovania|uskutočňovania))" + ":not(:containsOwn"
                        + "(Druh)) > span"});
    }

    /**
     * Parse award criterion list from document.
     *
     * @param document document to be parsed
     *
     * @return award criterion list or Null
     */
    private List<ParsedAwardCriterion> parseAwardCriteria(final Document document) {
        Element firstLine = document.select(IN_PART_IV + "div:has(span.code:matchesOwn(IV.2.1[\\.\\)]))").first();
        Element lastLine = document.select(IN_PART_IV + "div:has(span.code:matchesOwn(IV.(3|2.2)))").first();

        final List<ParsedAwardCriterion> parsedAwardCriteria = new ArrayList<>();

        if (firstLine != null && lastLine != null) {
            Element criteriaBatch = ParserUtils.getSubsectionOfElements(firstLine, lastLine);

            if (criteriaBatch == null) {
                return null;
            }

            List<String> criteria = getValuesFromElement(criteriaBatch, "div" +
                    ".shorttext_1e60f717fde90baf473fc7c874d5bfd535a18195ad8b2c762cbd8818fbc9076a");

            if (criteria == null || criteria.isEmpty()) {
                return null;
            }

            for (String criterion : criteria) {
                final String[] criterionParts = criterion.split("-");

                if (criterionParts.length == 0) {
                    return null;
                }

                final String criterionName = criterionParts[0].trim();
                String criterionWeight = null;
                if (criterionParts.length > 1) {
                    criterionWeight = criterionParts[1].trim();
                }

                parsedAwardCriteria.add(
                        new ParsedAwardCriterion().setName(criterionName).setWeight(criterionWeight));
            }
        } else {
            firstLine = document.select(IN_PART_IV + "div:has(span.code:matchesOwn(IV.1.1[\\.\\)]))").first();

            if (firstLine == null || lastLine == null) {
                return null;
            }

            Element criteriaBatch = ParserUtils.getSubsectionOfElements(firstLine, lastLine);

            if (criteriaBatch == null) {
                return null;
            }

            final List<String> names = getValuesFromElement(criteriaBatch, "div:containsOwn(Názov) > span");
            final List<String> weights = getValuesFromElement(criteriaBatch, "div:containsOwn(Váha) > span");

            if (names != null && !names.isEmpty()) {
                for (int i = 0; i < names.size(); i++) {

                    String weight = null;
                    if (weights != null) {
                        weight = i < weights.size() ? weights.get(i) : null;
                    }

                    parsedAwardCriteria.add(new ParsedAwardCriterion()
                            .setName(names.get(i))
                            .setWeight(weight));
                }
            }
        }

        return parsedAwardCriteria.isEmpty() ? null : parsedAwardCriteria;
    }

    /**
     * Parse publication source info value from document.
     *
     * @param document document to be parsed
     *
     * @return String[]
     */
    private String[] parsePublicationSourceInfo(final Document document) {
        String sourceIdAndFormType = getFirstValueFromElement(document, "div.mainHeader");

        return sourceIdAndFormType != null ? sourceIdAndFormType.split("-") : null;
    }
}
