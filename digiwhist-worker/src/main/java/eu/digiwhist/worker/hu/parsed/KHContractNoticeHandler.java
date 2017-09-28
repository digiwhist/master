package eu.digiwhist.worker.hu.parsed;

import static eu.digiwhist.worker.hu.parsed.KHTenderParserUtils.getAnnexADiv;
import static eu.digiwhist.worker.hu.parsed.KHTenderParserUtils.getOwnTextFromElement;
import static eu.digiwhist.worker.hu.parsed.KHTenderParserUtils.getSectionIIDiv;
import static eu.digiwhist.worker.hu.parsed.KHTenderParserUtils.getSectionIIIDiv;
import static eu.digiwhist.worker.hu.parsed.KHTenderParserUtils.getSectionIVDiv;
import static eu.digiwhist.worker.hu.parsed.KHTenderParserUtils.getSectionVDiv;
import static eu.digiwhist.worker.hu.parsed.KHTenderParserUtils.getSectionVIDiv;
import static eu.digiwhist.worker.hu.parsed.KHTenderParserUtils.getTextFromElement;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;

/**
 * Handler for Contract Notice on KH.
 *
 * @author Marek Mikes
 */
final class KHContractNoticeHandler {
    private static final String SUBSECTION_II_IN_ANNEX_A_VERSION_1 = "div:contains(II\\) Címek és " +
            "kapcsolattartási pontok, ahonnan a dokumentáció és a kiegészítő iratok (a versenypárbeszédre " + "és a "
            + "dinamikus beszerzési rendszerre vonatkozók is) beszerezhetők)";

    private static final String SUBSECTION_II_IN_ANNEX_A_VERSION_2 = "II\\) Címek és kapcsolattartási pontok, " +
            "ahonnan a dokumentáció és a kiegészítő iratok beszerezhetők";

    /**
     * Private constructor to make this class static.
     */
    private KHContractNoticeHandler() {
    }

    /**
     * Parses Contract Notice specific attributes and updates the passed tender.
     *
     * @param parsedTender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page
     *
     * @return updated tender object with data parsed from Contract Notice form
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document form) {
        parsedTender
                .setIsDps(parseIsTenderDps(form))
                .setIsFrameworkAgreement(parseIsTenderFrameworkAgreement(form))
                .setDescription(parseTenderDescription(form))
                .setCpvs(parseTenderCpvs(form))
                .setLots(parseTenderLots(form))
                .setIsCoveredByGpa(parseIsTenderCoveredByGpa(form))
                .setAreVariantsAccepted(parseAreVariantsAccepted(form))
                .setEstimatedPrice(new ParsedPrice()
                        .setNetAmount(parseTenderEstimatedNetAmount(form))
                        .setCurrency(parseTenderEstimatedPriceCurrency(form)))
                .setHasOptions(parseIfTenderHasOptions(form))
                .setEstimatedDurationInMonths(parseTenderEstimatedDurationInMonths(form))
                .setEstimatedDurationInDays(parseTenderEstimatedDurationInDays(form))
                .setEstimatedStartDate(parseTenderEstimatedStartDate(form))
                .setEstimatedCompletionDate(parseTenderEstimatedCompletionDate(form))
                .setPersonalRequirements(parseTenderPersonalRequirements(form))
                .setEconomicRequirements(parseTenderEconomicRequirements(form))
                .setTechnicalRequirements(parseTenderTechnicalRequirements(form))
                .setMaxBidsCount(parseTenderMaxBidsCount(form))
                .setSelectionMethod(parseTenderSelectionMethod(form))
                .setAwardCriteria(parseTenderAwardCriteria(form))
                .setIsElectronicAuction(parseIfTenderIsElectronicAuction(form))
                .setDocumentsDeadline(parseTenderDocumentsDeadline(form))
                .setDocumentsPayable(parseIfTenderDocumentsPayable(form))
                .setBidDeadline(parseBidDeadline(form))
                .setEligibleBidLanguages(parseTenderEligibleBidLanguages(form))
                .setAwardDeadline(parseTenderAwardDeadline(form))
                .addFunding(new ParsedFunding()
                        .setIsEuFund(parseIsTenderEuFund(form))
                        .setProgramme(parseTenderProgramme(form)))
                .setAppealBodyName(parseTenderAppealBodyName(form))
                .setMediationBodyName(parseTenderMediationBodyName(form))
                .setDocumentsLocation(new ParsedAddress()
                        .setStreet(parseStreetOfDocumentsLocation(form))
                        .setCity(parseCityOfDocumentsLocation(form))
                        .setPostcode(parsePostcodeOfDocumentsLocation(form))
                        .setCountry(parseCountryOfDocumentsLocation(form)));
        parsedTender.getPublications().get(0)
                .setDispatchDate(parsePublicationDispatchDate(form));
        return parsedTender;
    }

    /**
     * Parse if tender is dynamic purchasing system (DPS).
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIsTenderDps(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div:containsOwn(Dinamikus beszerzési rendszer (DBR) létrehozása) > span:nth-child(1)",
                        "div:containsOwn(A hirdetmény dinamikus beszerzési rendszer (DBR) létrehozására irányul) > "
                                + "span:nth-child(1)"});
    }

    /**
     * Parse if tender is awarded as superior framework agreement.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIsTenderFrameworkAgreement(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div:containsOwn(Keretmegállapodás megkötése) > span:nth-child(1)", "div:containsOwn(A "
                        + "hirdetmény keretmegállapodás megkötésére irányul) > span:nth-child(1)"});
    }

    /**
     * Parse tender description value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderDescription(final Document form) {
        return getTextFromElement(getSectionIIDiv(form), "div:has(div > div:contains(II.1.5)) + div span");
    }

    /**
     * Parse CPVs of publication from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return List of parsed CPVs
     */
    private static List<ParsedCPV> parseTenderCpvs(final Document form) {
        final List<ParsedCPV> cpvs = new ArrayList();

        // main CPV
        cpvs.add(new ParsedCPV().setCode(getTextFromElement(getSectionIIDiv(form),
                "div:has(div:containsOwn(II.1.6)) + div tr:nth-child(2) > td:nth-child(2)"))
                .setIsMain(Boolean.TRUE.toString()));

        // other CPVs
        String additionalObjectsString = getTextFromElement(getSectionIIDiv(form),
                "div:has(div:containsOwn(II.1.6)) + div tr:nth-child(3) > td:nth-child(2)");
        // sometimes the row does not exist (see 8734/2014)
        if (additionalObjectsString != null) {
            assert !additionalObjectsString.isEmpty();
            String[] additionalObjectStringList = additionalObjectsString.split(" ");
            for (final String additionalObjectString : additionalObjectStringList) {
                cpvs.add(new ParsedCPV().setCode(additionalObjectString).setIsMain(Boolean.FALSE.toString()));
            }
        }

        assert !cpvs.isEmpty();

        return cpvs;
    }

    /**
     * Parses number for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotNumber(final Element lotDiv) {
        return getTextFromElement(lotDiv,
                new String[]{"div:containsOwn(A rész száma) > span", "div:containsOwn(Rész száma) > span:nth-child(1)"
                        + ""});
    }

    /**
     * Parses title for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotTitle(final Element lotDiv) {
        return getTextFromElement(lotDiv, "div:containsOwn(Elnevezés:) > span:nth-child(2)");
    }

    /**
     * Parses description for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotDescription(final Element lotDiv) {
        return getTextFromElement(lotDiv, "div:contains(1\\)) + div span");
    }

    /**
     * Parses estimated net amount for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotEstimatedNetAmount(final Element lotDiv) {
        return getTextFromElement(lotDiv,
                new String[]{"div:containsOwn(Ha ismert, becsült érték ÁFA nélkül (csak számokkal):) > span",
                        "div:containsOwn(Becsült érték áfa nélkül:) > span:nth-child(1)"});
    }

    /**
     * Parses estimated price currency for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotEstimatedPriceCurrency(final Element lotDiv) {
        return getTextFromElement(lotDiv,
                new String[]{"div:contains(3\\)) + div + div + div span", "div:containsOwn(Becsült érték áfa nélkül:)"
                        + " > span:nth-child(2)"});
    }

    /**
     * Parses estimated duration in months for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotEstimatedDurationInMonths(final Element lotDiv) {
        return getTextFromElement(lotDiv,
                new String[]{"div:contains(4\\)) + div span", "div:containsOwn(Az időtartam hónapban:) > "
                        + "span:nth-child(1)"});
    }

    /**
     * Parses estimated duration in days for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotEstimatedDurationInDays(final Element lotDiv) {
        return getTextFromElement(lotDiv,
                new String[]{"div:contains(4\\)) + div + div span", "div:containsOwn(Az időtartam hónapban:) > "
                        + "span:nth-child(2)"});
    }

    /**
     * Parses estimated start date for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotEstimatedStartDate(final Element lotDiv) {
        return getTextFromElement(lotDiv, "div:contains(4\\)) + div + div + div span");
    }

    /**
     * Parses estimated completion date for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return String or Null
     */
    private static String parseLotEstimatedCompletionDate(final Element lotDiv) {
        return getTextFromElement(lotDiv, "div:contains(4\\)) + div + div + div + div span");
    }

    /**
     * Parses CPV codes for contract notice lot.
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return list of parsed CPV codes
     */
    private static List<ParsedCPV> parseLotCpvs(final Element lotDiv) {
        final List<ParsedCPV> cpvs = new ArrayList<>();

        // main CPV
        cpvs.add(new ParsedCPV().setIsMain(Boolean.TRUE.toString())
                .setCode(getTextFromElement(lotDiv,
                        "div:has(div:containsOwn(2\\))) + div tr:nth-child(2) > td:nth-child(2)")));

        // other CPVs
        String additionalObjectsString = getTextFromElement(lotDiv,
                "div:has(div:containsOwn(2\\))) + div tr:nth-child(3) > td:nth-child(2)");
        // sometimes the row does not exist and sometimes the row is empty (see 8734/2014 and 32411/2011)
        if (additionalObjectsString != null && !additionalObjectsString.isEmpty()) {
            String[] additionalObjectStringList = additionalObjectsString.split(" ");
            for (final String additionalObjectString : additionalObjectStringList) {
                cpvs.add(new ParsedCPV().setCode(additionalObjectString).setIsMain(Boolean.FALSE.toString()));
            }
        }

        return cpvs;
    }

    /**
     * Parses all the lots from contract notice.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseTenderLots(final Document form) {
        return getLotsDivs(form).stream().map(KHContractNoticeHandler::parseLot).collect(Collectors.toList());
    }

    /**
     * Finds tender lots (B attachments) in source HTML page and returns list of appropriate forms (one for each
     * lot/B attachment). If the lot is not embedded in a separate {@code form} element, wrapping {@code form}
     * element is created automatically.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return list of lot forms or empty list if no lots specified
     */
    private static Elements getLotsDivs(final Document form) {
        return form.select("div.content.hirdetmenytartalom > div > div:last-child > div");
    }

    /**
     * Parses tender lot from given lot form (HTML fragment).
     *
     * @param lotDiv
     *         parsed tender lot form
     *
     * @return parsed tender lot
     */
    private static ParsedTenderLot parseLot(final Element lotDiv) {
        return new ParsedTenderLot().setLotNumber(parseLotNumber(lotDiv))
                .setTitle(parseLotTitle(lotDiv))
                .setDescription(parseLotDescription(lotDiv))
                .setEstimatedPrice(new ParsedPrice().setNetAmount(parseLotEstimatedNetAmount(lotDiv))
                        .setCurrency(parseLotEstimatedPriceCurrency(lotDiv)))
                .setEstimatedDurationInMonths(parseLotEstimatedDurationInMonths(lotDiv))
                .setEstimatedDurationInDays(parseLotEstimatedDurationInDays(lotDiv))
                .setEstimatedStartDate(parseLotEstimatedStartDate(lotDiv))
                .setEstimatedCompletionDate(parseLotEstimatedCompletionDate(lotDiv))
                .setCpvs(parseLotCpvs(lotDiv));
    }

    /**
     * Parse if tender is covered by GPA.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIsTenderCoveredByGpa(final Document form) {
        // we can not parse by subsection number, because some contract notices does not have it (for example
        // "Eljárást megindító felhívás - 121. § (1) bekezdés b) pontja/KÉ/2011.12.30 KÉ")
        return getTextFromElement(getSectionIIDiv(form),
                "div:containsOwn(A szerződés a Közbeszerzési " + "Megállapodás (GPA) hatálya alá tartozik-e?) > span");
    }

    /**
     * Parse if variants are accepted value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseAreVariantsAccepted(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div:containsOwn(Elfogadhatók-e változatok (alternatív ajánlatok)?) > span",
                        "div:containsOwn(Elfogadhatók változatok (alternatív ajánlatok)) > span"});
    }

    /**
     * Parse tender estimated net amount value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedNetAmount(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div:containsOwn(Ha ismert, becsült érték ÁFA nélkül (csak számokkal):) > span",
                        "div:containsOwn(Becsült érték áfa nélkül:) > span:nth-child(1)"});
    }

    /**
     * Parse tender estimated price currency value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedPriceCurrency(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div > div:has(div > div:contains(Ha ismert, becsült érték ÁFA nélkül (csak számokkal):)"
                        + ") " + "+ div span", "div:containsOwn(Becsült érték áfa nélkül:) > span:nth-child(2)"});
    }

    /**
     * Parse if tender has options value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIfTenderHasOptions(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div:containsOwn(Vételi jog (opció) (adott esetben)) > span", "div:containsOwn(Vételi "
                        + "jog (opció):) > span"});
    }

    /**
     * Parse tender estimated duration in months value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedDurationInMonths(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div:containsOwn(Az időtartam hónap(ok)ban:) > span", "div:containsOwn(A (tervezett) "
                        + "időtartam hónapban:) > span:nth-child(1)"});
    }

    /**
     * Parse tender estimated duration in days value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedDurationInDays(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div > div:has(div > div:contains(Az időtartam hónap(ok)ban:)) + div span",
                        "div:containsOwn(A (tervezett) időtartam hónapban:) > span:nth-child(2)"});
    }

    /**
     * Parse tender estimated start date value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedStartDate(final Document form) {
        return getTextFromElement(getSectionIIDiv(form),
                new String[]{"div:containsOwn(VAGY: kezdés) > span", "div:containsOwn(Kezdés) > span"});
    }

    /**
     * Parse tender estimated completion date value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedCompletionDate(final Document form) {
        // there are two titles ("befejezés " and " Befejezés ") of the information and selector is case insensitive,
        // so there is only one selector
        return getTextFromElement(getSectionIIDiv(form), "div:containsOwn(befejezés) > span");
    }

    /**
     * Parse tender personal requirements value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderPersonalRequirements(final Document form) {
        return getTextFromElement(getSectionIIIDiv(form), "div:contains(III.2.1) + div + div span");
    }

    /**
     * Parse tender economic requirements value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEconomicRequirements(final Document form) {
        Element sectionIIIDiv = getSectionIIIDiv(form);
        return getTextFromElement(sectionIIIDiv, "div:contains(III.2.2) + div + div span") + getTextFromElement(
                sectionIIIDiv, "div:contains(III.2.2) + div + div + div + div span");
    }

    /**
     * Parse tender technical requirements value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderTechnicalRequirements(final Document form) {
        Element sectionIIIDiv = getSectionIIIDiv(form);
        return getTextFromElement(sectionIIIDiv, "div:contains(III.2.3) + div + div span") + getTextFromElement(
                sectionIIIDiv, "div:contains(III.2.3) + div + div + div + div span");
    }

    /**
     * Parse tender maximum number of bids value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderMaxBidsCount(final Document form) {
        return getTextFromElement(getSectionIVDiv(form),
                "div:containsOwn(és ( adott esetben) maximális létszáma) > span:nth-child(2)");
    }

    /**
     * Parse tender selection method value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderSelectionMethod(final Document form) {
        return getOwnTextFromElement(getSectionIIDiv(form), new String[]{
                // the lowest price option:
                "div:contains(IV.2.1) + div > div > div:has(span > b)",
                // the most economically advantageous tender option:
                "div:contains(IV.2.1) + div + div + div > div > div:has(span > b)"});
    }

    /**
     * Parse tender award criterion list from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return award criterion list
     */
    private static List<ParsedAwardCriterion> parseTenderAwardCriteria(final Document form) {
        final List<ParsedAwardCriterion> parsedTenderAwardCriteria = new ArrayList<>();

        Elements tenderAwardCriteriaTableRows = getSectionIVDiv(form).select(
                "table:has(th:containsOwn(Szempont)) tr:has(td)");
        for (Element tenderAwardCriteriaTableRow : tenderAwardCriteriaTableRows) {
            parsedTenderAwardCriteria.add(new ParsedAwardCriterion().setName(
                    tenderAwardCriteriaTableRow.select("td:nth-child(1)").text())
                    .setWeight(tenderAwardCriteriaTableRow.select("td:nth-child(2)").text()));
        }

        return parsedTenderAwardCriteria;
    }

    /**
     * Parse if tender is electronic auction value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIfTenderIsElectronicAuction(final Document form) {
        return getTextFromElement(getSectionIVDiv(form),
                new String[]{"div:containsOwn(Elektronikus árverést alkalmaznak-e?) > span", "div:containsOwn"
                        + "(Elektronikus árlejtés fognak alkalmazni) > span"});
    }

    /**
     * Parse documents deadline value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderDocumentsDeadline(final Document form) {
        return getTextFromElement(getSectionIVDiv(form),
                "div:contains(A dokumentáció beszerzésének határideje) + div span");
    }

    /**
     * Parse if tender documents are payable value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIfTenderDocumentsPayable(final Document form) {
        return getTextFromElement(getSectionIVDiv(form),
                new String[]{"div:containsOwn(Kell-e fizetni a dokumentációért?) > span", "div:containsOwn(A "
                        + "dokumentációért fizetni kell) > span"});
    }

    /**
     * Parse bid deadline value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBidDeadline(final Document form) {
        Element sectionIVDiv = getSectionIVDiv(form);
        final String subsectionTitle1 = "IV.3.4\\) Az ajánlattételi határidő, illetve a részvételi határidő";
        String bidDeadlineString = getTextFromElement(sectionIVDiv,
                "div:contains(" + subsectionTitle1 + ") + div span") + " " + getTextFromElement(sectionIVDiv,
                "div:contains(" + subsectionTitle1 + ") + div + div span");
        return bidDeadlineString;
    }

    /**
     * Parse tender eligible bid languages value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String[] or Null
     */
    private static List<String> parseTenderEligibleBidLanguages(final Document form) {
        // todo: refactor when we get forms where many languages are present

        List<String> eligibleBidLanguageStrings = new ArrayList<>();

        Element sectionIVDiv = getSectionIVDiv(form);
        if (sectionIVDiv.select("div:contains(IV.3.6) + div:contains(Az EU bármely hivatalos nyelve)").isEmpty()) {
            eligibleBidLanguageStrings.add(getTextFromElement(sectionIVDiv, "div:contains(IV.3.6) + div span"));
        } else {
            eligibleBidLanguageStrings.add(getTextFromElement(sectionIVDiv,
                    "div:contains(IV.3.6) + div + div + div + div > div > div > span:nth-child(2)"));
        }

        return eligibleBidLanguageStrings;
    }

    /**
     * Parse contract award deadline value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderAwardDeadline(final Document form) {
        return getTextFromElement(getSectionIVDiv(form), "div:contains(IV.3.7) + div span");
    }

    /**
     * Parse if tender is from EU funds.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIsTenderEuFund(final Document form) {
        String isEuFundString = getTextFromElement(getSectionVIDiv(form),
                "div:containsOwn(a szerződés eu-alapokból finanszírozott projekttel és/vagy programmal " +
                        "kapcsolatos?) > span");
        if (isEuFundString == null) {
            isEuFundString = getTextFromElement(getSectionVDiv(form),
                    "div:contains(A szerződés Európai Uniós alapokból finanszírozott projekttel és/vagy " +
                            "programmal kapcsolatos) + div span");
        }
        return isEuFundString;
    }

    /**
     * Parse tender fund programme value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderProgramme(final Document form) {
        String programmeString = getTextFromElement(getSectionVIDiv(form),
                "div:contains(Igen válasz esetén kérjük feltüntetni a projekt(ek) és/vagy program(ok) " + "nevét és "
                        + "bármely egyéb használható hivatkozási alapot:) + div span");
        if (programmeString == null) {
            programmeString = getTextFromElement(getSectionVDiv(form),
                    "div:contains((Igen válasz esetén) Hivatkozás a projekt(ek)re és/vagy program(ok)ra:) " + "+ div "
                            + "span");
        }
        return programmeString;
    }

    /**
     * Parse tender appeal body name value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderAppealBodyName(final Document form) {
        return getTextFromElement(getSectionVIDiv(form), "div:contains(VI.4.1) + div span");
    }

    /**
     * Parse tender mediation body name value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderMediationBodyName(final Document form) {
        return getTextFromElement(getSectionVIDiv(form),
                "div:contains(A békéltetési eljárást lebonyolító szerv (adott esetben)) + div span");
    }

    /**
     * Parse publication dispatch date value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parsePublicationDispatchDate(final Document form) {
        return getTextFromElement(getSectionVDiv(form),
                new String[]{"div:contains(VI.5) + div span", "div:containsOwn(V.5) > span"});
    }

    /**
     * Parse street of documents location value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseStreetOfDocumentsLocation(final Document form) {
        return getTextFromElement(getAnnexADiv(form),
                new String[]{SUBSECTION_II_IN_ANNEX_A_VERSION_1 + " + div + div span",
                        SUBSECTION_II_IN_ANNEX_A_VERSION_2 + " + div + div span"});
    }

    /**
     * Parse city of documents location value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseCityOfDocumentsLocation(final Document form) {
        return getTextFromElement(getAnnexADiv(form),
                new String[]{SUBSECTION_II_IN_ANNEX_A_VERSION_1 + " + div + div + div span",
                        SUBSECTION_II_IN_ANNEX_A_VERSION_2 + " + div + div + div span"});
    }

    /**
     * Parse postcode of documents location value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parsePostcodeOfDocumentsLocation(final Document form) {
        return getTextFromElement(getAnnexADiv(form),
                new String[]{SUBSECTION_II_IN_ANNEX_A_VERSION_1 + " + div + div + div + div span",
                        SUBSECTION_II_IN_ANNEX_A_VERSION_2 + " + div + div + div + div span"});
    }

    /**
     * Parse country of documents location value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseCountryOfDocumentsLocation(final Document form) {
        return getTextFromElement(getAnnexADiv(form),
                new String[]{SUBSECTION_II_IN_ANNEX_A_VERSION_1 + " + div + div + div + div + div span",
                        SUBSECTION_II_IN_ANNEX_A_VERSION_2 + " + div + div + div + div + div span"});
    }
}
