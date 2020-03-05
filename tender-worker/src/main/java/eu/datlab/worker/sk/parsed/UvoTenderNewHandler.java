package eu.datlab.worker.sk.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.cz.parsed.VestnikTenderParserUtils;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstOwnValueFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueWithoutDots;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFormType;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getNpwpReasons;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getTrueOrFalseFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getValuesFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Tender new form parsed for Slovakia.
 *
 * @author Michal Riha
 */
final class UvoTenderNewHandler {
    private static final Logger logger = LoggerFactory.getLogger(VestnikTenderParserUtils.class);

    private static final String IN_PART_I = "legend:matchesOwn(ODDIEL I[\\.:].*) + div ";
    private static final String IN_PART_II = "legend:matchesOwn(ODDIEL II.*) + div ";
    private static final String IN_PART_III = "legend:matchesOwn(ODDIEL III.*) + div ";
    private static final String IN_PART_IV = "legend:matchesOwn(ODDIEL IV.*) + div ";
    private static final String IN_PART_VI = "legend:matchesOwn(ODDIEL VI.*) + div ";

    private static final String APPENDIX_D1 = "legend:matchesOwn(PRÍLOHA D1.*) + div";

    private static final String SOURCE_DOMAIN = "https://www.uvo.gov.sk";

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
        // parse common attributes
        ParsedTender parsedTender = new ParsedTender().addPublication(
                new ParsedPublication()
                        .setSource(PublicationSources.SK_UVO)
                        .setSourceId(parsePublicationSourceId(document))
                        .setPublicationDate(publicationDate)
                        .setSourceFormType(parsePublicationSourceFormType(document))
                        .setDispatchDate(parsePublicationDispatchDate(document))
                        .setBuyerAssignedId(parseBuyerAssignedId(document))
                        .setHumanReadableUrl(url)
                        .setIsIncluded(true)
                        .setLanguage("SK"))
                .addPublication(parsePreviousPublication(document))
                .setNationalProcedureType(parseTenderNationalProcedureType(document))
                .setSupplyType(parseTenderSupplyType(document))
                .setEconomicRequirements(parseEconomicRequirements(document))
                .setTechnicalRequirements(parseTechnicalRequirements(document))
                .setSelectionMethod(parseTenderSelectionMethod(document))
                .setIsJointProcurement(parseIsJointProcurement(document))
                .setIsCentralProcurement(parseIsCentralProcurement(document))
                .setIsFrameworkAgreement(parseIsFrameworkAgreement(document))
                .addBuyer(new ParsedBody().setName(parseBuyerName(document))
                        .addBodyId(new BodyIdentifier().setId(parseBuyerBodyIdentifierId(document))
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                .setScope(BodyIdentifier.Scope.SK))
                        .setAddress(new ParsedAddress().setRawAddress(parseBuyerRawAddress(document))
                                .addNuts(getFirstValueFromElement(document, "span:containsOwn(Kód NUTS:) + span"))
                                .setCountry(parseBuyerCountry(document))
                                .setUrl(parseBuyerWebAddress(document)))
                        .setContactPoint(parseBuyerContactPoint(document))
                        .setContactName(parseBuyerContactName(document))
                        .setPhone(parseBuyerPhone(document))
                        .setEmail(parseBuyerEmail(document))
                        .setBuyerType(parseBuyerType(document))
                        .setMainActivities(parseBuyerMainActivities(document)))
                .setTitle(parseTenderTitle(document))
                .setAddressOfImplementation(
                        new ParsedAddress()
                                .setRawAddress(parseRawAddressOfImplementation(document))
                                .addNuts(parseNutsOfImplementation(document)))
                .setDescription(parseTenderDescription(document))
                .setCpvs(parseTenderNotMainCpvs(document))
                .addCpv(new ParsedCPV().setCode(parseTenderMainCpv(document)).setIsMain(String.valueOf(true)))
                .setIsCoveredByGpa(parseIsTenderCoveredByGpa(document))
                .setHasLots(parseIfTenderHasLots(document))
                .setAreVariantsAccepted(parseAreVariantsAccepted(document))
                .setEstimatedPrice(parseEstimatedPrice(document))
                .setHasOptions(parseIfTenderHasOptions(document))
                .setEstimatedDurationInMonths(parseTenderEstimatedDurationInMonths(document))
                .setEstimatedDurationInDays(parseTenderEstimatedDurationInDays(document))
                .setAwardCriteria(parseAwardCriteria(document))
                .setAwardDeadline(parseAwardDeadline(document))
                .setIsElectronicAuction(parseIfTenderIsElectronicAuction(document))
                .setBidDeadline(parseBidDeadline(document))
                .addFunding(new ParsedFunding()
                    .setIsEuFund(parseIsEUFunded(document))
                    .setProgramme(JsoupUtils.selectText("div:containsOwn(Číslo projektu alebo referenčné číslo) > span", document)))
                .setDocumentsPayable(parseIfTenderDocumentsPayable(document))
                .addPublications(parseRelatedPublications(document))
                .setEligibleBidLanguages(parseTenderEligibleBidLanguages(document))
                .setMediationBodyName(parseMediationBodyName(document))
                .setAwardDecisionDate(parseAwardDecisionDate(document))
                .setAppealBodyName(parseAppealBodyName(document))
                .setIsDps(parseIsDPS(document))
                .setNpwpReasons(parseNpwpReasons(document));

        //         parse form specific attributes
        PublicationFormType formType = getFormType(parsedTender.getPublications().get(0).getSourceFormType());
        switch (formType) {
            case CONTRACT_NOTICE:
                parsedTender = UvoTenderNewOzHandler.parse(parsedTender, document);
                break;
            case CONTRACT_AWARD:
                parsedTender = UvoTenderNewOzzHandler.parse(parsedTender, document);
                break;
            case CONTRACT_CANCELLATION:
                parsedTender = UvoTenderNewZzzHandler.parse(parsedTender, document);
                break;
            case CONTRACT_AMENDMENT:
                // first parse the form as CONTRACT_AWARD
                parsedTender = UvoTenderNewOzzHandler.parse(parsedTender, document);
                // then parse AMENDMENT details (section VII.)
                parsedTender = UvoTenderNewAmendmentHandler.parse(parsedTender, document);
                break;
            default:
                logger.warn("Unknown publication form type.");
        }

        return new ArrayList<>(Arrays.asList(parsedTender));
    }

    /**
     * Parse NPWP reasons.
     *
     * @param document document
     * @return List<String>
     */
    private List<String> parseNpwpReasons(final Document document) {
        Element section = document.selectFirst(IN_PART_IV + "div.subtitle:contains(IV.1.1)");
        if (section == null) {
            return null;
        }

        Element rawReasons;
        if (section.nextElementSibling().text().contains("vyplňte prosím prílohu D")) {
            rawReasons = document.selectFirst(APPENDIX_D1);
        } else {
            rawReasons = document.selectFirst(IN_PART_IV + "div.subtitle:contains(IV.1.1) + div + div");
        }

        return rawReasons != null ? getNpwpReasons(rawReasons.text()) : null;
    }

    /**
     * Parse decision date.
     *
     * @param document document
     * @return String
     */
    private String parseAwardDecisionDate(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                "div.subtitle:contains(DÁTUM ROZHODNUTIA O ZADAN) + div",
                "div.subtitle:contains(átum uzatvorenia zml) + div"});
    }


    /**
     * Parse tender appeal body name value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseAppealBodyName(final Document document) {
        Element appealBodyName = document.select(IN_PART_VI + "div:has(span:containsOwn(Orgán zodpovedný za " +
                "preskúmanie)) + div > span").first();

        if (appealBodyName == null) {
            return null;
        } else {
            return appealBodyName.html().split("<br>")[0].replaceAll("\\<[^)]*\\>", "");
        }
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
        Element node = JsoupUtils.selectFirst(IN_PART_IV + "div:containsOwn(zahŕňa uzavretie rámcovej dohody)", document);
        if (node == null) {
            return null;
        }
        // case when the existence of node means isFrameworkAgreement = TRUE
        if (node.children().isEmpty()) {
            return Boolean.TRUE.toString();
        }

        return getTrueOrFalseFromElement(node, "span");
    }

    /**
     * Parse estimated price.
     *
     * @param document document to parse from
     *
     * @return return estimated price
     */
    private ParsedPrice parseEstimatedPrice(final Document document) {
        return parsePrice(document,
            IN_PART_II + "div.subtitle:has(span:matchesOwn(Celková (odhadovaná|predpokladaná) hodnota)) + div > span + span + span",
            IN_PART_II + "div.subtitle:has(span:matchesOwn(Celková (odhadovaná|predpokladaná) hodnota)) + div > span",
            IN_PART_II + "div.subtitle:has(span:matchesOwn(Celková (odhadovaná|predpokladaná) hodnota)) + div > span + span",
            null);
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
                IN_PART_II + "div:containsOwn(financovaného z fondov Európskej únie) > span",
                IN_PART_II + "span:containsOwn(financovaného z fondov Európskej únie) + span"});
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
                "(inimálna lehota, počas ktorej sú ponuky uchádzačov viaz)) ~ div:containsOwn(Ponuka musí " +
                "platiť do:)");

        if (awarddeadline != null) {
            return awarddeadline.replace("Ponuka musí platiť do:", "").replace("Dátum:", "").replace("Čas:", "");
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
    private ParsedPublication parsePreviousPublication(final Document document) {
        final String previousPublicationSourceId = getFirstValueFromElement(document, IN_PART_IV + "div:containsOwn"
                + "(Číslo oznámenia v Ú. v. EÚ) span");
        String previousPublicationDate = getFirstValueFromElement(document, IN_PART_IV + "div:containsOwn(Číslo " +
                "oznámenia v Ú. v. EÚ) + div:containsOwn(z:)");

        if (previousPublicationDate != null) {
            previousPublicationDate = previousPublicationDate.replace("z:", "");
        }

        return previousPublicationSourceId == null
                ? null : new ParsedPublication()
                .setSourceId(previousPublicationSourceId)
                .setSource(PublicationSources.EU_TED)
                .setPublicationDate(previousPublicationDate)
                .setIsIncluded(false);
    }

    /**
     * Parse economic requirements.
     *
     * @param document document to parse from
     *
     * @return return economic requirements
     */
    private String parseEconomicRequirements(final Document document) {
        return getFirstValueFromElement(document, IN_PART_III + "div:has(span:containsOwn(Ekonomické a finančné " +
                "postavenie)) + div + div:not(:contains(III.))");
    }

    /**
     * Parse technical requirements.
     *
     * @param document document to parse from
     *
     * @return return technical requirements
     */
    private String parseTechnicalRequirements(final Document document) {
        return getFirstValueFromElement(document, IN_PART_III + "div:has(span:containsOwn(Technická a odborná " +
                "spôsobilosť)) + div + div:not(:contains(III.))");
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
                IN_PART_II + " div > span:containsOwn(NUTS) ~ div",
                IN_PART_II + "div:has(span:containsOwn(NUTS)) + div > div"});
    }

    /**
     * Parse value from document.
     *
     * @param document document to be parsed
     *
     * @return List<ParsedCpv> or Null
     */
    private List<ParsedCPV> parseTenderNotMainCpvs(final Document document) {
        List<String> cpvCodes = new ArrayList<>();

        final Element firstLineOfSubsection = document.select(
                IN_PART_II + "div:has(span.code:matchesOwn(^II\\.2\\.2[\\.\\)]))").first();
        final Element lastLineOfSubsection = document.select(
                IN_PART_II + "div:has(span.code:matchesOwn(^II\\.2\\.(3|4|5)[\\.\\)]))").first();

        if (firstLineOfSubsection != null && lastLineOfSubsection != null) {
            Element subsection = ParserUtils.getSubsectionOfElements(firstLineOfSubsection, lastLineOfSubsection);

            if (subsection != null) {
                cpvCodes = getValuesFromElement(subsection, "div.selectList > span:not(.title)");
            }
        }

        String codes = getFirstValueFromElement(document,
            IN_PART_II + "div.cpvSelectList div:containsOwn(Doplňujúce predmety) + div");

        if (codes != null) {
            cpvCodes.addAll(Arrays.asList(codes.replace("Hlavný slovník:", "").trim().split(",")));
        }

        if (cpvCodes == null) {
            return null;
        }

        List<ParsedCPV> parsedCPVs = new ArrayList<>();

        for (String cpvCode : cpvCodes) {
            parsedCPVs.add(new ParsedCPV()
                    .setCode(cpvCode.trim().replaceAll("\\.+$", ""))
                    .setIsMain(String.valueOf(false)));
        }

        return parsedCPVs;
    }

    /**
     * Parse tender main CPV value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderMainCpv(final Document document) {
        String cpvCode = getFirstValueFromElement(document, new String[]{
            IN_PART_II + "div:has(span:containsOwn(Hlavný kód CPV)) + div",
            IN_PART_II + "div.cpvSelectList div span:containsOwn(Hlavný predmet) ~ span",
        });

        if (cpvCode == null) {
            return null;
        } else {
            return cpvCode.trim().replaceAll("\\.+$", "").replaceAll("Hlavný slovník: ?", "");
        }
    }

    /**
     * Parse tender eligible bid languages value from document.
     *
     * @param document document to be parsed
     *
     * @return String[] or Null
     */
    private List<String> parseTenderEligibleBidLanguages(final Document document) {
        Elements nodes = JsoupUtils.select(IN_PART_IV + "div:has(span:containsOwn(Jazyk (jazyky), v ktorom (ktorých) možno predkladať))" +
            " + div > div", document);

        if (nodes == null) {
            return null;
        }

        return nodes.stream()
            .flatMap(n -> Stream.of(n.text().split(",")))
            .collect(Collectors.toList());
    }

    /**
     * Parse if tender documents are payable value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderDocumentsPayable(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "div:has(span:containsOwn(KOMUNIKÁCIA)) + div");
    }

    /**
     * Parse bid deadline value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseBidDeadline(final Document document) {
        String bidDeadline = getFirstValueFromElement(document,
                IN_PART_IV + "div:has(span:matchesOwn(Lehota na predkladanie (ponúk|návrhov))) + div" +
                        ":not(:contains(Leta))");

        if (bidDeadline == null) {
            return null;
        }

        return bidDeadline.replace("Dátum a čas:", "");
    }

    /**
     * Parse if tender is electronic auction value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderIsElectronicAuction(final Document document) {
        Element node = JsoupUtils.selectFirst(IN_PART_IV + "div:containsOwn(Použije sa elektronická aukcia)", document);
        if (node == null) {
            return null;
        }
        // case when the existence of node means isElectronicAction = TRUE
        if (node.children().isEmpty()) {
            return Boolean.TRUE.toString();
        }

        return getTrueOrFalseFromElement(node, "span");
    }

    /**
     * Parse tender estimated duration in months value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderEstimatedDurationInMonths(final Document document) {
        return getFirstValueFromElement(document, new String[]{
            IN_PART_II + "div:has(span:containsOwn(Dĺžka trvania zákazky)) + div + div:containsOwn(mesiacoch) > span",
            IN_PART_II + "div:has(span:containsOwn(Dĺžka trvania zákazky)) + div:has(span:containsOwn(mesiacoch)) + div > span"
        });
    }

    /**
     * Parse tender estimated duration in days value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderEstimatedDurationInDays(final Document document) {
        return getFirstValueFromElement(document, new String[]{
            IN_PART_II + "div:has(span:containsOwn(Dĺžka trvania zákazky)) + div + div:containsOwn(dňoch) > span",
            IN_PART_II + "div:has(span:containsOwn(Dĺžka trvania zákazky)) + div:has(span:containsOwn(dňoch)) + div > span"
        });
    }

    /**
     * Parse if tender has options value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderHasOptions(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_II + "div:has(span:containsOwn(Informácie o opciách)) + div > span + span");
    }

    /**
     * Parse if variants are accepted value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseAreVariantsAccepted(final Document document) {
        return getFirstValueFromElement(document, IN_PART_II + "span:containsOwn(Budú sa akceptovať varianty) + span");
    }

    /**
     * Parse if tender has lots value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderHasLots(final Document document) {
        return getTrueOrFalseFromElement(document,
                IN_PART_II + "div:containsOwn(zákazka sa delí na čast) > span");
    }

    /**
     * Parse tender national procedure type value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderNationalProcedureType(final Document document) {
        String procedureType = getFirstOwnValueFromElement(document, new String[]{
                IN_PART_IV + "div:has(span.code:containsOwn(IV.1.) + span:containsOwn(druh)) + div span",
                IN_PART_IV + "div:has(span.code:containsOwn(IV.1.) + span:containsOwn(druh)) + div",
                "div.notice > div:has(strong:containsOwn(Druh postupu:):not(:containsOwn(Osoby)))"});

        if (procedureType == null) {
            return null;
        } else {
            return procedureType.replace("Druh postupu", "").replaceAll("\\.+$", "").replaceAll(":", "");
        }
    }

    /**
     * Parse tender selection method from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderSelectionMethod(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_IV + "span:matchesOwn((nomicky najvýhodnejšia ponuka z hľadiska|ajnižšia cena|ižšie uvedené " +
                        "kritéri)):not(:matchesOwn(-|%|ritérium)):not(:matchesOwn(^\\d))",
                "div:containsOwn(Kritéria kvality)",
                IN_PART_II + "span:matchesOwn(ižšie uvedené kritéri):not(:matchesOwn(-|%|ritérium))" +
                        ":not(:matchesOwn(^\\d))",
                "div:containsOwn(Kritériá kvality)",
                "div.subtitle:contains(Kritériá na vyhodnotenie ponúk) + div"
        });
    }

    /**
     * Parse tender supply type value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderSupplyType(final Document document) {
        return getFirstOwnValueFromElement(document, new String[]{
                "div.notice > div:has(strong:containsOwn(Druh zákazky:))",
                IN_PART_II + "div:has(span:containsOwn(Druh zákazky)) + div:not(:contains(II.)) > span"});
    }

    /**
     * Parse tender title value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderTitle(final Document document) {
        return getFirstValueFromElement(document, IN_PART_II + "div:has(span:containsOwn(Názov)) + div");
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
        return getFirstValueFromElement(document, IN_PART_II + "div:containsOwn(Referenčné číslo:) > span");
    }

    /**
     * Parse if tender is covered by GPA.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseIsTenderCoveredByGpa(final Document document) {
        return getTrueOrFalseFromElement(document,
                IN_PART_IV + "span:containsOwn(obstarávanie sa vzťahuje dohoda o vládnom obstarávaní) + span");
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
        return getFirstValueFromElement(document, IN_PART_I + "span:containsOwn((URL)) + span");
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
            IN_PART_I +"span:containsOwn(Druh verejného obstarávateľa:) + span",
            IN_PART_I +"div:has(span:containsOwn(DRUH VEREJNÉHO OBSTARÁVATEĹA)) ~ div.selectList > span:not(.title)"
        });
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

        final List<String> mainActivities = getValuesFromElement(root, new String[]{"div > div", "div + div"});
        if (mainActivities != null) {
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
        if (otherActivities != null) {
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
     * Parse raw address of implementation value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private String parseRawAddressOfImplementation(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_II + "span:containsOwn(Hlavné miesto dodania alebo plnenia:) + span",
                IN_PART_II + "div:has(span:containsOwn(Hlavné miesto dodania alebo plnenia:)) + div"});
    }

    /**
     * Parse award criterion list from document.
     *
     * @param document document to be parsed
     *
     * @return award criterion list or Null
     */
    private List<ParsedAwardCriterion> parseAwardCriteria(final Document document) {
        final Element firstLineOfSubsection = document.select(
                IN_PART_II + "div:has(span.code:matchesOwn(^II\\.2\\.5[\\.\\)]))").first();
        final Element lastLineOfSubsection = document.select(
                IN_PART_II + "div:has(span.code:matchesOwn(^II\\.2\\.(6|7|8|9|10|11|12|13)[\\.\\)]))").first();

        if (firstLineOfSubsection == null || lastLineOfSubsection == null) {
            return null;
        }

        Element subsection = ParserUtils.getSubsectionOfElements(firstLineOfSubsection, lastLineOfSubsection);

        if (subsection == null) {
            return null;
        }

        final List<ParsedAwardCriterion> result = new ArrayList<>();

        if (!subsection.text().contains("Nižšie uvedené kritéria")) {
            final Elements weights = subsection.select("div:contains(Relatívna váh)");

            for (Element weightElement : weights) {
                final Element name = weightElement.previousElementSibling();
                final Element weight = weightElement.selectFirst("span");

                result.add(new ParsedAwardCriterion()
                        .setName(name == null ? null : name.text().replace("Názov:", ""))
                        .setWeight(weight == null ? null : weight.text())
                );
            }

            return result;
        } else {
            for (Element criterion : getCriteriaSubsections(subsection)) {
                final String name = getFirstOwnValueFromElement(criterion, "div:containsOwn(Názov) > span");

                if (name != null && !name.trim().isEmpty()) {
                    result.add(new ParsedAwardCriterion()
                            .setName(name)
                            .setWeight(getFirstOwnValueFromElement(criterion, "div:containsOwn(váha) > span")));
                }
            }

            if (!result.isEmpty()) {
                return result;
            }
        }

        final Element anotherPossibleAward = document.selectFirst("span:containsOwn(Náklady/Cena) + span");

        if (anotherPossibleAward != null) {
            result.add(new ParsedAwardCriterion()
                    .setName(anotherPossibleAward.text())
                    .setWeight("100")
            );

            return result;
        }

        return null;
    }

    /**
     * Parse publication source info value from document.
     *
     * @param document document to be parsed
     *
     * @return String[]
     */
    private String[] parsePublicationSourceInfo(final Document document) {
        String sourceIdAndFormType = getFirstValueFromElement(document, new String[]{"div.mainHeader", "div.MainHeader"});
        return sourceIdAndFormType != null ? sourceIdAndFormType.split("-") : null;
    }

    /**
     * Create element for each lot in OZ form.
     *
     * @param document element to be parsed from
     *
     * @return List<Element>
     */
    private static List<Element> getCriteriaSubsections(final Element document) {
        List<Element> criteriaFirstLines = document.select("span:containsOwn(Časť:)");

        if (criteriaFirstLines == null || criteriaFirstLines.isEmpty()) {
            return Arrays.asList(document);
        }

        // Few publications have multiple designations of one criteria, remove those
        String previousLine = "";
        for (int iterator = 0; iterator < criteriaFirstLines.size();) {
            if (criteriaFirstLines.get(iterator).toString().equals(previousLine)) {
                criteriaFirstLines.remove(iterator);
            } else {
                previousLine = criteriaFirstLines.get(iterator).toString();
                iterator++;
            }
        }

        List<Element> subsections = new ArrayList<>();

        for (int iterator = 0; iterator < criteriaFirstLines.size(); iterator++) {
            if ((iterator + 1) != criteriaFirstLines.size()) {
                subsections.add(ParserUtils.getSubsectionOfElements(criteriaFirstLines.get(iterator),
                        criteriaFirstLines.get(iterator + 1)));
            } else {
                subsections.add(ParserUtils.getSubsectionOfElements(criteriaFirstLines.get(iterator), null));
            }
        }

        return subsections;
    }

    /**
     * Parse if tender is DPS.
     *
     * @param document document to parse from
     *
     * @return return isDPS value
     */
    private String parseIsDPS(final Document document) {
        return getTrueOrFalseFromElement(document, new String[]{
            IN_PART_IV + "div:containsOwn(Dynamický nákupný systém môžu využiť ďalší zákazníci) > span",
            IN_PART_IV + "span:containsOwn(Dynamický nákupný systém môžu využiť ďalší zákazníci) + span"});
    }
}
