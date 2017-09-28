package eu.digiwhist.worker.sk.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.cz.parsed.VestnikTenderParserUtils;
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
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getFirstValueWithoutDots;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getFormType;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getTrueOrFalseFromElement;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getValuesFromElement;
import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

/**
 * Tender ancient form parsed for Slovakia.
 *
 * @author Michal Riha
 */
final class UvoTenderAncientHandler {

    private static final Logger logger = LoggerFactory.getLogger(VestnikTenderParserUtils.class);

    private static final String IN_PART_I = "span.nadpis:matchesOwn(ODDIEL I[\\.:].*) + table ";
    private static final String IN_PART_II = "span.nadpis:matchesOwn(ODDIEL II.*) + table ";
    private static final String IN_PART_III = "span.nadpis:matchesOwn(ODDIEL III.*) + table ";
    private static final String IN_PART_IV = "span.nadpis:matchesOwn(ODDIEL IV.*) + table ";
    private static final String IN_PART_VI = "span.nadpis:matchesOwn(ODDIEL VI.*) + table ";

    private static final String SOURCE_DOMAIN = "https://www.uvo.gov.sk";

    /**
     * Parse common data for all ancient forms.
     *
     * @param document
     *         document to parse data from
     * @param url
     *         url of the document
     * @param publicationDate
     *         publication date of the document
     *
     * @return List<ParsedTender> with parsed data
     */
    List<ParsedTender> parse(final Document document, final String url, final String publicationDate) {

        // parse common attributes
        ParsedTender parsedTender = new ParsedTender().addPublication(
                new ParsedPublication().setSourceId(parsePublicationSourceId(document))
                        .setSource(PublicationSources.SK_UVO)
                        .setPublicationDate(publicationDate)
                        .setSourceFormType(parsePublicationSourceFormType(document))
                        .setDispatchDate(parsePublicationDispatchDate(document))
                        .setBuyerAssignedId(parseBuyerAssignedId(document))
                        .setHumanReadableUrl(url)
                        .setIsIncluded(true)
                        .setLanguage("SK"))
                .addPublications(parsePreviousPublications(document))
                .setNationalProcedureType(parseTenderNationalProcedureType(document))
                .setSupplyType(parseTenderSupplyType(document))
                .setSelectionMethod(parseTenderSelectionMethod(document))
                .setPersonalRequirements(parsePersonalRequirements(document))
                .setEconomicRequirements(parseEconomicRequirements(document))
                .setTechnicalRequirements(parseTechnicalRequirements(document))
                .setIsFrameworkAgreement(parseIsFrameworkAgreement(document))
                .addBuyer(new ParsedBody().setName(parseBuyerName(document))
                        .addBodyId(new BodyIdentifier().setId(parseBuyerBodyIdentifierId(document))
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                .setScope(BodyIdentifier.Scope.SK))
                        .setAddress(new ParsedAddress().setRawAddress(parseBuyerRawAddress(document))
                                .setStreet(parseBuyerStreet(document))
                                .setCity(parseBuyerCity(document))
                                .setPostcode(parseBuyerPostcode(document))
                                .setCountry(parseBuyerCountry(document))
                                .setUrl(parseBuyerWebAddress(document)))
                        .setContactPoint(parseBuyerContactPoint(document))
                        .setContactName(parseBuyerContactName(document))
                        .setPhone(parseBuyerPhone(document))
                        .setEmail(parseBuyerEmail(document))
                        .setBuyerType(parseBuyerType(document))
                        .addMainActivity(parseBuyerMainActivity(document)))
                .setIsOnBehalfOf(parseIsTenderOnBehalfOfSomeone(document))
                .setTitle(parseTenderTitle(document))
                .setAddressOfImplementation(
                        new ParsedAddress().setRawAddress(parseRawAddressOfImplementation(document))
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
                .setMediationBodyName(parseTenderMediationBodyName(document))
                .setFundings(Arrays.asList(new ParsedFunding()
                        .setIsEuFund(parseIsEUFunded(document))))
                .addPublications(parseRelatedPublications(document));

        //         parse form specific attributes
        PublicationFormType formType = getFormType(parsedTender.getPublications().get(0).getSourceFormType());
        switch (formType) {
            case CONTRACT_NOTICE:
                parsedTender = UvoTenderAncientOzHandler.parse(parsedTender, document);
                break;
            case CONTRACT_AWARD:
                parsedTender = UvoTenderAncientOzzHandler.parse(parsedTender, document);
                break;
            case CONTRACT_CANCELLATION:
                parsedTender = UvoTenderAncientZzzHandler.parse(parsedTender, document);
                break;
            default:
                logger.warn("Unknown publication form type.");
        }

        return new ArrayList<>(Arrays.asList(parsedTender));
    }

    /**
     * Parse if there is framework agreement.
     *
     * @param document
     *         document to be parsed
     *
     * @return string or null
     */
    private String parseIsFrameworkAgreement(final Document document) {
        String frameworkAgreement = getFirstValueFromElement(document, new String[]{
                IN_PART_II + "span:matchesOwn((verejnej zákazke|Oznámenie zahŕňa)) ~ span.hodnota",
                IN_PART_II + "tr:has(span:matchesOwn((verejnej zákazke|Oznámenie zahŕňa))) + tr span.hodnota"});

        if (frameworkAgreement == null) {
            return null;
        } else {
            return String.valueOf(frameworkAgreement.toLowerCase()
                    .contains("Vypracovanie rámcovej dohody".toLowerCase()));
        }
    }

    /**
     * Parse technical requirements.
     *
     * @param document
     *         document to parse from
     *
     * @return return technical requirements
     */
    private String parseTechnicalRequirements(final Document document) {
        return getFirstValueFromElement(document, IN_PART_III + "tr:has(span:containsOwn" +
                "(Technická alebo odborná spôsobilosť)) + tr span.hodnota");
    }

    /**
     * Parse economic requirements.
     *
     * @param document
     *         document to parse from
     *
     * @return return economic requirements
     */
    private String parseEconomicRequirements(final Document document) {
        return getFirstValueFromElement(document, IN_PART_III + "tr:has(span:containsOwn" +
                "(Ekonomické a finančné postavenie)) + tr span.hodnota");
    }

    /**
     * Parse personal requirements.
     *
     * @param document
     *         document to parse from
     *
     * @return return personal requirements
     */
    private String parsePersonalRequirements(final Document document) {
        return getFirstValueFromElement(document, IN_PART_III + "tr:has(span:containsOwn" +
                "(sobné postavenie uchádzačov a záujemcov vrátane poži)) + tr span.hodnota");
    }

    /**
     * Parse related publications.
     *
     * @param document
     *         document to parse from
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
     * @param document
     *         document to parse from
     *
     * @return return parsed value if tender is EU funded
     */
    private String parseIsEUFunded(final Document document) {
        return getTrueOrFalseFromElement(document, IN_PART_VI + "span:containsOwn(programu financovaného z fondov) ~ " +
                "span.hodnota");
    }

    /**
     * Parse award deadline.
     *
     * @param document
     *         document to parse from
     *
     * @return return parsed award deadline
     */
    private String parseAwardDeadline(final Document document) {
        String awardDeadline = getFirstValueFromElement(document, new String[]{
                IN_PART_IV + "tr:has(span.nazov:matchesOwn(Podmienky otvárania (ponúk|návrhov))) + tr span" +
                        ".hodnota:containsOwn(Dátum)",
                IN_PART_IV + "span.nazov:matchesOwn(Podmienky na otváranie obálok s ponukami) ~ span" +
                        ".hodnota:containsOwn(Dátum)",
                IN_PART_IV + "tr:has(span.nazov:matchesOwn(Podmienky na otváranie obálok s ponukami)) + tr span" +
                        ".hodnota:containsOwn(Dátum)"});

        if (awardDeadline == null) {
            return null;
        } else {
            return awardDeadline.replace("Dátum:", "").replace("Čas:", "");
        }
    }

    /**
     * Parse previous publications.
     *
     * @param document
     *         document to parse from
     *
     * @return return parsed previous publications
     */
    private List<ParsedPublication> parsePreviousPublications(final Document document) {
        final String areTherePreviousPublications = getFirstValueFromElement(document, new String[]{
                IN_PART_IV + "span.nazov:containsOwn(Predchádzajúce oznámenie) ~ span.hodnota",
                IN_PART_IV + "span.nazov:containsOwn(Predchádzajúce uverejnenie) ~ span.hodnota"});

        if (areTherePreviousPublications != null && areTherePreviousPublications.contains("Áno")) {
            final List<String> previsousRawPublications = getValuesFromElement(document, new String[]{
                    IN_PART_IV + "tr:has(span.nazov:containsOwn(Predchádzajúce oznámenie)) + " +
                            "tr span:containsOwn(Číslo oznámenia) ~ span.hodnota",
                    IN_PART_IV + "tr:has(span.nazov:containsOwn(Predchádzajúce uverejnenie)) + " +
                            "tr span:containsOwn(Číslo oznámenia) ~ span.hodnota",
                    IN_PART_IV + "span:containsOwn(v Ú. v. EÚ) + span.hodnota"});

            if (previsousRawPublications != null) {
                final List<ParsedPublication> parsedPublications = new ArrayList<>();

                for (String previousRawPublication : previsousRawPublications) {
                    String[] previousPublicationIdAndDate = previousRawPublication.split(" z ");

                    ParsedPublication parsedPublication = new ParsedPublication()
                            .setSource(PublicationSources.EU_TED)
                            .setSourceId(previousPublicationIdAndDate[0].replace(": ", ""))
                            .setIsIncluded(false);
                    if (previousPublicationIdAndDate.length > 1) {
                        parsedPublication.setPublicationDate(previousPublicationIdAndDate[1]);
                    }

                    parsedPublications.add(parsedPublication);
                }

                return parsedPublications.isEmpty() ? null : parsedPublications;
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    /**
     * Parse publication source ID value from document.
     *
     * @param document
     *         document to be parsed
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
     * @param document
     *         document to be parsed
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
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parsePublicationDispatchDate(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_VI + "span:matchesOwn(odoslania tohto oznámeni) ~ span.hodnota",
                IN_PART_VI + "tr:has(span:matchesOwn(ODOSLANI(E|A) TOHTO OZNÁMENIA)) + tr span.hodnota"});
    }

    /**
     * Parse NUTS code of implementation value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseNutsOfImplementation(final Document document) {
        return getFirstValueWithoutDots(document,
                IN_PART_II + "tr:has(td > span.podnazov:containsOwn(NUTS)) + tr span.hodnota");
    }

    /**
     * Parse  value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return List<ParsedCpv> or Null
     */
    private List<ParsedCPV> parseTenderNotMainCpvs(final Document document) {
        List<ParsedCPV> parsedCPVs = new ArrayList<>();
        String cpvCodesString = getFirstValueFromElement(document,
                "tr:has(td.kod:matchesOwn(^II\\.1\\.(6|5|3)[\\.\\)])) + tr + tr span.hodnota:containsOwn(Hlavný " +
                        "slovník)");

        if (cpvCodesString == null) {
            return null;
        }

        String[] cpvCodes = cpvCodesString.replace("Hlavný slovník:", "").replace("Doplňujúce predmety", "")
                .trim().split(",");

        for (String cpvCode : cpvCodes) {
            if (!cpvCode.isEmpty()) {
                parsedCPVs.add(new ParsedCPV()
                        .setCode(cpvCode.trim().replaceAll("\\.+$", ""))
                        .setIsMain(String.valueOf(false)));
            }
        }

        return parsedCPVs.isEmpty() ? null : parsedCPVs;
    }

    /**
     * Parse tender main CPV value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private ParsedCPV parseTenderMainCpv(final Document document) {
        String mainCpv = getFirstValueFromElement(document,
                IN_PART_II + "tr:has(td.kod:matchesOwn(^II\\.1\\.(6|5|4|3)[\\.\\)])) + tr span.hodnota:containsOwn" + ""
                        + "(Hlavný slovník) + span.hodnota");

        if (mainCpv == null) {
            return null;
        }

        mainCpv = mainCpv.replace("Hlavný slovník:", "").trim().replaceAll("\\.+$", "");

        return new ParsedCPV().setCode(mainCpv).setIsMain(String.valueOf(true));
    }

    /**
     * Parse tender appeal body name value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderMediationBodyName(final Document document) {
        return getFirstValueFromElement(document, IN_PART_VI + "tr:has(span:containsOwn(mediačné konanie)) + tr tr");
    }

    /**
     * Parse tender eligible bid languages value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String[] or Null
     */
    private List<String> parseTenderEligibleBidLanguages(final Document document) {
        return getValuesFromElement(document, IN_PART_IV + "span.nazov:containsOwn(Jazyk (jazyky)) + table");
    }

    /**
     * Parse tender enquiry deadline value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderBidDeadline(final Document document) {
        String bidDeadline = getFirstValueFromElement(document,
                IN_PART_IV + "tr:has(span.nazov:matchesOwn(Lehota na predkladanie (ponúk|návrhov))) + tr" +
                        ":not(:contains(Leta))");

        if (bidDeadline != null) {
            return bidDeadline.replace("Dátum:", "").replace("Čas:", "");
        } else {
            return null;
        }
    }

    /**
     * Parse tender documents price.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private ParsedPrice parseDocumentsPrice(final Document document) {
        return parsePrice(document, false,
                IN_PART_IV + "tr:has(td.kod:matchesOwn(^IV\\.3\\.3[\\.\\)])) ~ tr:has(span.podnazov:containsOwn(ena))" +
                        " span.hodnota",
                IN_PART_IV + "tr:has(td.kod:matchesOwn(^IV\\.3\\.3[\\.\\)])) ~ tr:has(span.podnazov:containsOwn(ena))" +
                        " span.podnazov:containsOwn(Mena) + span.hodnota", null);
    }

    /**
     * Parse if tender documents are payable value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderDocumentsPayable(final Document document) {
        return getFirstValueFromElement(document,
                new String[]{IN_PART_IV + "span.podnazov:matchesOwn(Spoplatnenie súťažných podkladov) ~ span"
                        + ".hodnota", IN_PART_IV + "span.podnazov:matchesOwn(Úhrada za súťažné podklady) ~ span"
                        + ".hodnota"});
    }

    /**
     * Parse bid deadline value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseDocumentsDeadline(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_IV + "tr:has(span.podnazov:containsOwn(Lehota na prijímanie žiadostí o súťažné" + " podklady)"
                        + ") + tr:contains(Dátum) > td > span.hodnota");
    }

    /**
     * Parse if tender is electronic auction value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderIsElectronicAuction(final Document document) {
        return getTrueOrFalseFromElement(document, new String[]{
                IN_PART_IV + "span:matchesOwn((Použije|Využila) sa elektronická aukcia) ~ span.hodnota",
                IN_PART_IV + "span:containsOwn(elektronickej aukcii) ~ span.hodnota"});
    }

    /**
     * Parse tender deposits value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderDeposits(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_III + "span.nazov:containsOwn(Požadované zábezpeky a záruky) ~ span.hodnota");
    }

    /**
     * Parse tender estimated duration in months value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderEstimatedDurationInMonths(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_II + "tr:has(td.kod:matchesOwn(^II\\.3[\\.\\)])) + tr:has(span.hodnota:containsOwn" + ""
                        + "(mesiacoch)) + tr span.hodnota");
    }

    /**
     * Parse if tender has options value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderHasOptions(final Document document) {
        return getFirstValueFromElement(document,
                new String[]{IN_PART_II + "span.nazov:containsOwn(Opcie) ~ span.hodnota", IN_PART_II + "tr:has(span"
                        + ".nazov:containsOwn(Opcie)) + tr > td > span.hodnota"});
    }

    /**
     * Parse tender estimated price.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private ParsedPrice parseEstimatedPrice(final Document document) {
        return parsePrice(document,
                IN_PART_II + "span.podnazov:containsOwn(predpokladan)",
                IN_PART_II + "tr:has(span.podnazov:containsOwn(predpokladan)) + tr > td > span.hodnota",
                IN_PART_II + "tr:has(span.podnazov:containsOwn(predpokladan)) + tr > td > span.hodnota + span" +
                        ".hodnota", null);
    }

    /**
     * Parse if variants are accepted value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseAreVariantsAccepted(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_II + "span:matchesOwn((V|v)ariant(né|ných|y)) ~ span.hodnota",
                IN_PART_II + "tr:has(span:matchesOwn((V|v)ariant(né|ných|y))) + tr span.hodnota"});
    }

    /**
     * Parse if tender has lots value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseIfTenderHasLots(final Document document) {
        String hasLots = getFirstValueFromElement(document,
                new String[]{IN_PART_II + "span.nazov:containsOwn(Rozdelenie na časti) ~ span.hodnota", IN_PART_II +
                        "tr:has(span.nazov:containsOwn(Rozdelenie na časti)) + tr > td > span.hodnota"});

        if (hasLots == null) {
            return null;
        } else if (hasLots.contains("Áno")) {
            return String.valueOf(true);
        } else if (hasLots.contains("Nie")) {
            return String.valueOf(false);
        } else {
            return null;
        }
    }

    /**
     * Parse tender national procedure type value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderNationalProcedureType(final Document document) {
        String procedureType = getFirstValueFromElement(document, new String[]{
                IN_PART_IV + "td.kod:matchesOwn(^IV\\.1[\\.\\)]) + td > span:containsOwn(Druh) ~ span.hodnota",
                IN_PART_IV + "tr:has(td.kod:matchesOwn(^IV\\.1[\\.\\)]) + td:contains(DRUH SÚŤAŽE NÁVRHOV)) + tr",
                "td.cast span.nazov:containsOwn(Druh postupu) ~ span.hodnota:not(:containsOwn(Osoby))"});

        if (procedureType == null) {
            return null;
        } else {
            return procedureType.replace("Druh postupu", "").replaceAll("\\.+$", "").replaceAll(":", "");
        }
    }

    /**
     * Parse tender selection method from document.
     *
     * @param document
     *         document to be parsed
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
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderSupplyType(final Document document) {
        return getFirstValueFromElement(document, "td.cast span.nazov:containsOwn(Druh zákazky) ~ span.hodnota");
    }

    /**
     * Parse tender title value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderTitle(final Document document) {
        return getFirstValueFromElement(document,
                new String[]{IN_PART_II + "span.nazov:matchesOwn(Názov (zákazky|súťaže|pridelený zákazke)) ~ span"
                        + ".hodnota", IN_PART_II + "tr:has(span.nazov:matchesOwn(Názov (zákazky|súťaže|pridelený "
                        + "zákazke)))" + " + tr span.hodnota"});
    }

    /**
     * Parse tender description value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseTenderDescription(final Document document) {
        return getFirstValueFromElement(document,
                new String[]{IN_PART_II + "span.nazov:containsOwn(Stručný opis) ~ span.hodnota", IN_PART_II + "tr:has"
                        + "(span.nazov:containsOwn(Stručný opis)) + tr > td > span.hodnota"});
    }

    /**
     * Parse buyer assigned ID value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerAssignedId(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_IV + "span.nazov:matchesOwn((Evidenčné|Referenčné) číslo spisu) ~ span.hodnota");
    }

    /**
     * Parse if tender is covered by GPA.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseIsTenderCoveredByGpa(final Document document) {
        return getTrueOrFalseFromElement(document, new String[]{
                IN_PART_II + "td:has(span.nazov:containsOwn(vládnom obstarávaní \\(GPA\\))) > span.hodnota",
                IN_PART_II + "tr:has(td:has(span.nazov:containsOwn(vládnom obstarávaní \\(GPA\\)))) + tr span" +
                        ".hodnota:not(:containsOwn(Na))",
                IN_PART_II + "tr:has(td:has(span.nazov:containsOwn(vládnom obstarávaní \\(GPA\\)))) + tr + tr span" +
                        ".hodnota"});
    }

    /**
     * Parse buyer name value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerName(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "tr:eq(1) tbody > tr:eq(0) span.hodnota");
    }

    /**
     * Parse buyer contact point value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerContactPoint(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_I + "tr > td.hodnota:matches(.*Kontaktné miesto.*) > span.hodnota");
    }

    /**
     * Parse buyer contact name value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerContactName(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_I + "tr > td.hodnota:containsOwn(Kontaktná osoba:) > span.hodnota");
    }

    /**
     * Parse buyer phone value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerPhone(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "tr > td.hodnota:containsOwn(Telefón:) > span.hodnota");
    }

    /**
     * Parse buyer email value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerEmail(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "tr > td.hodnota:matchesOwn(E-mail.*:) > span.hodnota");
    }

    /**
     * Parse buyer web address value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerWebAddress(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_I + "span:matchesOwn(\\(URL\\)|Internetová adresa) ~ span.hodnota");
    }

    /**
     * Parse buyer type value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerType(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_I + "span:containsOwn(Druh verejného obstarávateľa) ~ span.hodnota + span.hodnota",
                IN_PART_I + "span:containsOwn(TYP OBSTARÁVACEJ INŠTITÚCIE) ~ span.hodnota"});
    }

    /**
     * Parse buyer main activity value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerMainActivity(final Document document) {
        return getFirstValueFromElement(document, new String[]{
                IN_PART_I + "span:matchesOwn(Hlavn(ý|á) (predmet|činnosť)) + table span.hodnota:not(:matchesOwn(In" +
                        "(ý|é)))",
                IN_PART_I + "tr:has(span:matchesOwn(Hlavn(ý|á) (predmet|činnosť))) + tr span.hodnota:not(:matchesOwn" +
                        "(In(ý|é)))",
                IN_PART_I + "tr:has(span:matchesOwn(Hlavn(ý|á) (predmet|činnosť))) + tr + tr span.hodnota:not" +
                        "(:matchesOwn(In(ý|é)))",
                IN_PART_I + "span:matchesOwn(HLAVNÁ ČINNOSŤ) + table span.hodnota",
                IN_PART_I + "span:containsOwn(TYP OBSTARÁVACEJ INŠTITÚCIE) ~ span.hodnota"});
    }

    /**
     * Parse buyer body identifier ID value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerBodyIdentifierId(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "tr:eq(1) tbody > tr:eq(1) span.hodnota");
    }

    /**
     * Parse buyer raw address value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerRawAddress(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_I + "tr:eq(1) tbody tr:eq(2):not(:contains(Poštová adresa:)) span.hodnota");
    }

    /**
     * Parse buyer street value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerStreet(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_I + "tr:eq(1) tbody tr:eq(2) td.hodnota:containsOwn(Poštová adresa:) span.hodnota");
    }

    /**
     * Parse buyer city value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerCity(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_I + "tr > td.hodnota:containsOwn(Mesto/obec:) > span.hodnota");
    }

    /**
     * Parse buyer postcode value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerPostcode(final Document document) {
        return getFirstValueFromElement(document, IN_PART_I + "tr > td.hodnota:containsOwn(PSČ:) > span.hodnota");
    }

    /**
     * Parse buyer country value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseBuyerCountry(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_I + "tr > td.hodnota:matchesOwn((Štát|Krajina):.*) > span.hodnota");
    }

    /**
     * Parse if tender is made on behalf of someone value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseIsTenderOnBehalfOfSomeone(final Document document) {
        return getTrueOrFalseFromElement(document,
                IN_PART_I + "span:matchesOwn((?i).*obstarávateľ.*nakupuje.*iných" + ".*obstarávateľov.*) ~ span"
                        + ".hodnota");
    }

    /**
     * Parse raw address of implementation value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private String parseRawAddressOfImplementation(final Document document) {
        return getFirstValueFromElement(document, IN_PART_II + "span:containsOwn(Hlavné) ~ span.hodnota");
    }

    /**
     * Parse award criterion list from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return award criterion list or Null
     */
    private List<ParsedAwardCriterion> parseAwardCriteria(final Document document) {
        String maybeCriteriName = getFirstValueFromElement(document,
                IN_PART_IV + "td.kod:matchesOwn(^IV\\.2\\.1[\\.\\)]) + td > span.hodnota");

        if (maybeCriteriName == null) {
            maybeCriteriName = getFirstValueFromElement(document,
                    IN_PART_IV + "td.kod:matchesOwn(^IV\\.3[\\.\\)]) + td span.hodnota");
        }

        if (maybeCriteriName == null) {
            return null;
        }

        final List<ParsedAwardCriterion> parsedAwardCriteria = new ArrayList<>();

        if (maybeCriteriName.contains("Ekonomicky najvýhodnejšia ponuka z hľadisk")) {
            final List<String> criterions = getValuesFromElement(document,
                    IN_PART_IV + "tr:has(td.kod:matchesOwn(^IV\\.2\\.1[\\.\\)])) + tr + tr span.hodnota");

            if (criterions != null) {
                for (String criterion : criterions) {
                    final String[] criterionParts = criterion.split("-");

                    final String criterionName = criterionParts[0].trim();
                    String criterionWeight = null;
                    if (criterionParts.length > 1) {
                        criterionWeight = criterionParts[1].trim();
                    }

                    parsedAwardCriteria.add(
                            new ParsedAwardCriterion()
                                    .setName(criterionName)
                                    .setWeight(criterionWeight));
                }
            }
        } else {
            parsedAwardCriteria.add(new ParsedAwardCriterion().setName(maybeCriteriName));
        }

        for (ParsedAwardCriterion criterion : parsedAwardCriteria) {
            if (criterion.getName().toLowerCase().contains("najnižšia cena")) {
                criterion.setIsPriceRelated(String.valueOf(true));
            }
        }

        return parsedAwardCriteria;
    }

    /**
     * Parse publication source info value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String[]
     */
    private String[] parsePublicationSourceInfo(final Document document) {
        String sourceIdAndFormType = getFirstValueFromElement(document, "h2.document_number_and_code");

        return sourceIdAndFormType != null ? sourceIdAndFormType.split("-") : null;
    }
}
