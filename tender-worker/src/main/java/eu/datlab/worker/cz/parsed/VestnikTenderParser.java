package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.lang3.BooleanUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * This class is responsible for managing parsing of the raw tenders downloaded from old Vestnik.
 */
public final class VestnikTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "2.5";

    private static final String VESTNIK_SOURCE_URL = PublicationSources.CZ_VESTNIK;
    private static final String TED_SOURCE_URL = PublicationSources.EU_TED;

    private static final Set<String> SUPPORTED_FORMS = new HashSet<String>(
            Arrays.asList("2", "5", "9", "11", "12", "3", "6", "13", "15", "18", "51", "54"));

    private static final Logger logger = LoggerFactory.getLogger(VestnikTenderParser.class);

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
        final String formCode = VestnikTenderParserUtils.getFormType(rawTender);
        logger.info("Trying to parse form {} from {}.", formCode, rawTender.getSourceUrl());

        final Document form = Jsoup.parse(rawTender.getSourceData());
        ParsedTender tender;

        // parse common attributes
        if (isFormSupported(formCode)) {
            tender = parseCommonTenderInfo(form, formCode, rawTender);

            // parse form specific attributes with appropriate form handler
            switch (formCode) {
                case "2":
                    tender = VestnikForm2Handler.parseFormAttributes(tender, form);
                    break;
                case "5":
                    tender = VestnikForm5Handler.parseFormAttributes(tender, form);
                    break;
                case "9":
                    tender = VestnikForm9Handler.parseFormAttributes(tender, form);
                    break;
                case "11":
                    tender = VestnikForm11Handler.parseFormAttributes(tender, form);
                    break;
                case "12":
                    tender = VestnikForm12Handler.parseFormAttributes(tender, form);
                    break;
                case "3":
                    tender = VestnikForm3Handler.parseFormAttributes(tender, form);
                    break;
                case "6":
                    tender = VestnikForm6Handler.parseFormAttributes(tender, form);
                    break;
                case "13":
                    tender = VestnikForm13Handler.parseFormAttributes(tender, form);
                    break;
                case "15":
                    tender = VestnikForm15Handler.parseFormAttributes(tender, form);
                    break;
                case "18":
                    tender = VestnikForm18Handler.parseFormAttributes(tender, form);
                    break;
                case "51":
                    tender = VestnikForm51Handler.parseFormAttributes(tender, form);
                    break;
                case "54":
                    tender = VestnikForm54Handler.parseFormAttributes(tender, form);
                    break;
                default:
                    logger.error("No handler found for form code: {}", formCode);
                    break;
            }
        } else {
            logger.warn("Unsupported form {}.", formCode);
            // parse publication info even for unsupported forms
            tender = new ParsedTender().setPublications(parsePublications(form, rawTender));
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
     * Parses tender attributes from all the parts(sections) of given form that
     * are same(or very similar) for all the form types (that includes buyer
     * info, publications, cpv codes, award criteria, fundings etc.).
     *
     * @param form
     *         parsed document for the source HTML page
     * @param formCode
     *         form number which specifies the form type
     * @param rawTender
     *         raw tender object
     *
     * @return tender with parsed information from common sections
     */
    private static ParsedTender parseCommonTenderInfo(final Document form, final String formCode,
            final RawData rawTender) {
        if (form == null) {
            logger.error("Could not start parsing tender data. Source data could not be parsed.");
            throw new UnrecoverableException("Source data could not be parsed.");
        }

        // BASIC COMMON TENDER ATTRIBUTES
        final ParsedTender tender = new ParsedTender().setPublications(parsePublications(form, rawTender))
                .addBuyer(parseBuyer(form))
                .setTitle(VestnikTenderParserUtils.getTextAfterLabel(form,
                        "FormItems_((NazevPrideleny(Verejne)?Zakazce(((VerejnymZadavatelem|Koncesionarem)?(_II_1_1)?)"
                                + "" + "|SouteziONavrh_I_4_2))" + "|NazevZakazkyZadaneNaZakladeRamcoveSmlouvy_IV" +
                                "|NazevVerejneZakazkyPridelenyZadavatelem_II)"));

        // form 54 is completely different, all the other forms have couple of other common attributes
        if (!formCode.equals("54")) {
            tender.setIsOnBehalfOf(VestnikTenderParserUtils.getCheckedInputValue(form,
                    "FormItems\\.((ZakazkaJmenemJinych_I_(3|4))|"
                            + "(VerejnyZadavatelZadavaVerejnouZakazkuJmenemJinychZadavatelu_I_(2|4)))"))
                    .setDescription(VestnikTenderParserUtils.getTextAfterLabel(form,
                            "FormItems_StrucnyPopis(Verejne)?(Zakazky)?((SoutezeONavrh)|(NeboNakupu))?_((II(_)?((1_5)"
                                    + "" + "|(3)|(1_3)|(I_2)|(1_4)|(1_2)))?|(I_4_3)?)"))
                    .setNationalProcedureType(VestnikTenderParserUtils.getCheckedOptionLabel(form,
                            "FormItems\\.((DruhRizeni_IV_1(_1)?)|(DruhVerejneSouteze_IV_1)|"
                                    + "(DruhZadavacihoRizeniVerejneZakazky_I_5_1))"))
                    .setSupplyType(getFormSupplyType(form, formCode))
                    .setIsFrameworkAgreement(BooleanUtils.toStringTrueFalse(isFrameworkAgreement(form)))
                    .setIsCoveredByGpa(BooleanUtils.toStringTrueFalse(VestnikTenderParserUtils.isInputFieldChecked(form,
                            "FormItems\\.NaZakazkuSeVztahujeDohoda((OVerejnychZakazkach)|(OVZ))_II_(1_)?(6|7)")))
                    .setIsElectronicAuction(VestnikTenderParserUtils.getCheckedInputValue(form,
                            "FormItems\\.((((Bude)|(Byla))PouzitaElektronicka(Drazba|Aukce)_IV_(2|1)_2)|" +
                                    "(ElDrazba_IV_2_2))"))
                    .setAddressOfImplementation(parseAddressOfImplementation(form))
                    .setCpvs(parseCpvCodes(form))
                    .addFunding(parseEuFunding(form))
                    .setAdditionalInfo(parseAdditionalInfo(form));

            // SELECTION METHOD and AWARD CRITERIA
            final String selectionMethod = VestnikTenderParserUtils.getCheckedOptionLabel(form,
                    "FormItems\\.((NejnizsiNabidkovaCena)|(KriteriaProZadaniZakazky(1)?)|(KriteriaTyp))_IV_2_1");
            tender.setSelectionMethod(selectionMethod);
            // if MEAT, parse criteria
            if (selectionMethod != null && !selectionMethod.startsWith("Nejnižší nabídková cena")) {
                tender.setAwardCriteria(parseAwardCriteria(form));
            }
        }
        return tender;
    }

    /**
     * Checks whether the tender is/will be awarded as superior framework
     * agreement.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return true if the given form is(will be) awarded as superior framework
     * agreement, false if not and null if the information cannot be
     * found
     */
    static Boolean isFrameworkAgreement(final Document form) {
        return VestnikTenderParserUtils.isInputFieldChecked(form,
                "FormItems\\.((InformaceOVerejneZakazceRs)|((Toto)?OznameniSeTyka(Uzavreni)?RamcoveSmlouvy))_II_1_3");
    }

    /**
     * Returns the supply type of given form.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param formCode
     *         form code (form type number, eg. 2, 3, 51)
     *
     * @return type of supplies for given tender or null if the information
     * could not be determined
     */
    private static String getFormSupplyType(final Document form, final String formCode) {
        // form 11 is always for WORKS
        if (formCode.equals("11")) {
            return TenderSupplyType.WORKS.toString();
        }
        return VestnikTenderParserUtils.getCheckedOptionLabel(form,
                "FormItems\\.((DruhZakazky(AMistoProvadeniStavebnichPraci)?_II_(1_)?2)|(TotoZruseniSeTyka_I_4_1))");
    }

    /**
     * Parses information about tender buyer. This information is parsed from
     * subsection I.1) or 1).
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return tender buyer with all the parsed details
     */
    private static ParsedBody parseBuyer(final Document form) {
        final Element subsectionI1 = VestnikTenderParserUtils.getFormSubsectionByName(form,
                "^((I\\.)?1\\))|(ODDÍL 1:).*");
        // BUYER
        final ParsedBody buyer = VestnikTenderParserUtils.parseBody(subsectionI1);
        return buyer;
    }

    /**
     * Parses info about this publication and all the related and previous ones.
     * Source HTML for results of related Vestnik publications search is stored
     * within the {@code rawTender} object. Previous publications in TED (or
     * Vestnik) are usually provided in section IV.3.2). Publication of this
     * form in TED in mentioned in the form header section.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param rawTender
     *         raw tender with downloaded source data and metadata
     *
     * @return list of all parsed publications including previous ones and other
     * related ones
     */
    private static List<ParsedPublication> parsePublications(final Document form, final RawData rawTender) {
        final List<ParsedPublication> publications = new ArrayList<>();
        final String formCode = VestnikTenderParserUtils.getFormType(rawTender);
        final String sourceUrl = rawTender.getSourceUrl().toString();
        final HashMap<String, String> additionalUrls = (HashMap<String, String>) rawTender.getMetaData()
                .get("additionalUrls");

        // THIS PUBLICATION
        publications.add(parseThisPublication(form, formCode, sourceUrl));

        // THIS PUBLICATION IN TED (from form header)
        publications.add(parseThisPublicationInTed(form));

        // RELATED PUBLICATIONS IN VESTNIK (using searching for related forms)
        if (additionalUrls == null || additionalUrls.isEmpty()) {
            logger.error(
                    "Unable to parse related publications from Vestnik. Related forms page not found in metadata.");
        } else {
            final Document relatedPublicationsForm = Jsoup.parse(additionalUrls.values().iterator().next());
            publications.addAll(parseRelatedVestnikPublications(form, formCode, relatedPublicationsForm));
        }

        // PREVIOUS PUBLICATIONS IN TED AND VESTNIK (parsed from form, typically section IV.3.2)
        publications.addAll(parsePreviousPublications(form, formCode));

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
                .setSourceId(getFormId(form))
                .setSourceTenderId(VestnikTenderParserUtils.parseInfoFromHeader(form, "Evidenční číslo zakázky"))
                .setPublicationDate(VestnikTenderParserUtils.getFormPublicationDate(form))
                .setHumanReadableUrl(sourceUrl)
                .setDispatchDate(
                        VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.DatumOdeslani(_|TohotoOznameni_).*"))
                .setBuyerAssignedId(VestnikTenderParserUtils.getFieldValue(form,
                        "FormItems\\.((Spis(ove)?Cislo)|(SpisoveCisloPrideleneKoncesionarem)|(SpisoveCisloPridelene"
                                + "(Verejnym)?Zadavatelem))_IV_(1|2|3|4)_1"));
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
    private static ParsedPublication parseThisPublicationInTed(final Document form) {
        ParsedPublication publication = null;
        final String publicationDispatchDate = VestnikTenderParserUtils.parseInfoFromHeader(form,
                "Datum odeslání do TED");
        final String publicationInfo = VestnikTenderParserUtils.parseInfoFromHeader(form,
                "Číslo a datum zveřejnění v TED");
        if (publicationDispatchDate != null) {
            publication = new ParsedPublication().setSource(TED_SOURCE_URL)
                    .setIsIncluded(false)
                    .setDispatchDate(publicationDispatchDate);

            if (publicationInfo != null) {
                final String[] publicationDetails = publicationInfo.split("zveřejněno");
                publication.setSourceId(publicationDetails[0].trim()).setPublicationDate(publicationDetails[1].trim());
            }
        }
        return publication;
    }

    /**
     * Parses info about related publication in Vestnik, that is all forms that
     * has been published for this tender in Vestnik. These related forms are
     * found by using appropriate search functionality in Vestnik and the result
     * of this search is provided by crawler/downloader within the metadata in
     * the {@code rawObject}.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param originalFormCode
     *         form code of the primary publication (the included one)
     * @param relatedPublicationsForm
     *         parsed document tree for the list of related publications page
     *
     * @return list of publications in Vestnik related to the same tender or
     * empty list if no such forms found
     */
    private static List<ParsedPublication> parseRelatedVestnikPublications(final Document form,
            final String originalFormCode, final Document relatedPublicationsForm) {
        final List<ParsedPublication> publications = new ArrayList<>();
        final Elements relatedPublicationsElements = relatedPublicationsForm.select(
                "div#content div#SearchGrid > table > tbody > tr");
        for (final Element publicationElement : relatedPublicationsElements) {
            final Elements publicationDetails = publicationElement.select("td");
            final String publicationSourceId = publicationDetails.get(1).text();
            // skip this publication
            if (!publicationSourceId.equals(getFormId(form))) {
                String relatedPublicationFormCode = publicationDetails.get(4).text();
                publications.add(new ParsedPublication().setIsIncluded(false)
                        .setSource(VESTNIK_SOURCE_URL)
                        .setSourceFormType(relatedPublicationFormCode)
                        .setSourceId(publicationSourceId)
                        .setPublicationDate(publicationDetails.get(5).text())
                        .setHumanReadableUrl(publicationDetails.get(1).select("a").attr("abs:href"))
                        .setIsValid(!publicationElement.hasClass("old"))
                        .setIsParentTender(originalFormCode.equals("54") && !relatedPublicationFormCode.equals("54")));
            }
        }
        return publications;
    }

    /**
     * Parses information about previous publications (usually in TED) related to
     * the same tender. This information is typically provided in the IV.3.2
     * subsection.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param formCode
     *         form number which specifies the form type
     *
     * @return list of previous publications related to the same tender or empty
     * list if no such publications found
     */
    private static List<ParsedPublication> parsePreviousPublications(final Document form, final String formCode) {
        final List<ParsedPublication> publications = new ArrayList<>();
        if (Arrays.asList("11", "12", "54").contains(formCode)) {
            // do nothing, there are no previous publications
            return publications;
        }
        switch (formCode) {
            case "9":
                publications.add(parsePreviousPublication(form, "FormItems\\.CisloOznameniVUrednimVestniku_IV_2_2",
                        "FormItems\\.ZeDne_IV_2_2", TED_SOURCE_URL));
                break;
            case "51":
                // previous TED publication
                publications.add(parsePreviousPublication(form,
                        "FormItems\\.VerejnaZakazkaBylaUverejnenaVUrednimVestnikuEUPodEvidencnimCislem_I_6_2",
                        "FormItems\\.Dne_I_6_2", TED_SOURCE_URL));
                // previous Vestnik publication
                publications.add(parsePreviousPublication(form,
                        "FormItems\\.VerejnaZakazkaBylaUverejnenaPodEvidencnimCislem_I_6_1", "FormItems\\.Dne_I_6_1",
                        VESTNIK_SOURCE_URL));
                break;
            default:
                final String hasPreviousPublications = VestnikTenderParserUtils.getCheckedInputValue(form,
                        "FormItems\\.PredchoziZverejneni(((TykajiciSeStejneZakazky)?_IV_3_2)|" + "" + "" + "" + "" +
                                "(TykajiciSeStejneSouteze_IV_1_2))");
                // only parse previous publications if it's checked that there are some
                if (hasPreviousPublications != null && hasPreviousPublications.equalsIgnoreCase("true")) {
                    if (formCode.equals("13")) {
                        publications.add(parsePreviousPublication(form, "FormItems\\.CisloOznameniVUrVest_IV_1_2",
                                "FormItems\\.ZeDne_IV_1_2", TED_SOURCE_URL));
                    } else {
                        // prior information notice or notice on a buyer profile
                        publications.addAll(parsePreviousTedPublications(form,
                                "FormItems\\.(OznamPredbezInformaci|OznameniPredbeznychInformaci|" +
                                        "PredchoziZverejneniDruh)_IV_3_2"));
                        // prior tender notice
                        publications.addAll(
                                parsePreviousTedPublications(form, "FormItems\\.OznameniOZakazce(Druh)?(_IV_3_2)?"));
                        // voluntary ex ante transparency notice
                        publications.addAll(parsePreviousTedPublications(form,
                                "FormItems\\.Oznameni(ODobrovolnePruhlednosti)?ExAnte_IV_3_2"));
                        // other previous publications
                        publications.addAll(
                                parsePreviousTedPublications(form, "FormItems\\.JinaPredchozi(U|Z)verejneni_IV_3_2"));
                    }
                }
                break;
        }
        return publications;
    }

    /**
     * Parses details about previously published form related to the same
     * tender. This function is used when there is no information about previous
     * form type (no checkbox or radio button specifying the form type).
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param publicationSourceIdElementRegex
     *         regular expression for the {@code name} attribute of element
     *         containing previous publication source ID
     * @param publicationDateElementRegex
     *         regular expression for the {@code name} attribute of element
     *         containing previous form publication date
     * @param source
     *         source where the previous publication has been published
     *
     * @return parsed info about previously published form related to the same
     * tender or null if no such info found.
     */
    private static ParsedPublication parsePreviousPublication(final Document form,
            final String publicationSourceIdElementRegex, final String publicationDateElementRegex,
            final String source) {
        final String publicationSourceId = VestnikTenderParserUtils.getFieldValue(form,
                publicationSourceIdElementRegex);
        final String publicationDate = VestnikTenderParserUtils.getFieldValue(form, publicationDateElementRegex);
        if (((publicationSourceId != null) && !publicationSourceId.isEmpty()) || ((publicationDate != null)
                && !publicationDate.isEmpty())) {
            return new ParsedPublication().setIsIncluded(false)
                    .setSource(source)
                    .setSourceId(publicationSourceId)
                    .setPublicationDate(publicationDate);
        }
        return null;
    }

    /**
     * Parses details about previously published forms in TED related to the
     * same tender which are of specified type. It checks whether the buyer
     * selected the option of previously published form for the specified form
     * type and if so, parses the details.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param publicationTypeNameAttrRegex
     *         regular expression for the {@code name} attribute of checkbox
     *         or radio button input element for selecting previous
     *         publication form type
     *
     * @return List of publication details for forms previously published in TED
     * wich are of specified type. Empty list is returned when no such
     * publications.
     */
    private static List<ParsedPublication> parsePreviousTedPublications(final Document form,
            final String publicationTypeNameAttrRegex) {
        final List<ParsedPublication> publications = new ArrayList<>();
        final Elements publicationDetailDivs = form.select(
                String.format("div.radio:has(div.iform-field > input[name~=%s][checked]) ~ div.left",
                        publicationTypeNameAttrRegex));
        if (publicationDetailDivs.size() >= 2) {
            final Elements publicationSourceIdElements = publicationDetailDivs.get(0).select("div.iform-field > input");
            final Elements publicationDateElements = publicationDetailDivs.get(1).select("div.iform-field > input");
            for (int i = 0; i < publicationSourceIdElements.size(); i++) {
                publications.add(new ParsedPublication().setIsIncluded(false)
                        .setSource(TED_SOURCE_URL)
                        .setSourceFormType(
                                VestnikTenderParserUtils.getCheckedOptionLabel(form, publicationTypeNameAttrRegex))
                        .setSourceId(VestnikTenderParserUtils.getElementContent(publicationSourceIdElements.get(i)))
                        .setPublicationDate(
                                VestnikTenderParserUtils.getElementContent(publicationDateElements.get(i))));
            }
        }
        return publications;
    }

    /**
     * Parses address of tender implementation.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return address of implementations with provided NUTS codes
     */
    private static ParsedAddress parseAddressOfImplementation(final Document form) {
        final ParsedAddress addressOfImplementation = new ParsedAddress().setRawAddress(
                VestnikTenderParserUtils.getTextAfterLabel(form,
                        "FormItems_HlavniMisto(ProvadeniStavebnichPraci)?_II_1_2"));

        for (final Element nutsElement : form.select("input[name~=FormItems\\.(Kod)?((?i)Nuts)[1-4]_II_1_2]")) {
            final String nutsCode = VestnikTenderParserUtils.getElementContent(nutsElement);
            if (nutsCode != null && !nutsCode.isEmpty()) {
                addressOfImplementation.addNuts(nutsCode);
            }
        }
        return addressOfImplementation;
    }

    /**
     * Parses CPV codes.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return list of parsed CPV codes
     */
    private static List<ParsedCPV> parseCpvCodes(final Document form) {
        final List<ParsedCPV> cpvs = new ArrayList<>();
        // main CPV
        cpvs.add(new ParsedCPV().setIsMain(Boolean.TRUE.toString())
                .setCode(VestnikTenderParserUtils.getFieldValue(form,
                        "FormItems\\.HlavniSlovnikHp_((II_((4)|(1_[3-6])))|(I_4_4))")));

        // other CPVs
        cpvs.addAll(form.select(
                "div.iform-field > input[name~=FormItems\\.HlavniSlovnikDp[1-4]_((II_((4)|(1_[3-6])))|(I_4_4))]")
                .stream()
                .filter(cpvDetail -> !cpvDetail.attr("value").isEmpty())
                .map(cpvDetail -> new ParsedCPV().setIsMain(Boolean.FALSE.toString()).setCode(cpvDetail.attr("value")))
                .collect(Collectors.toList()));
        return cpvs;
    }

    /**
     * Parses all the award criteria and their weight.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return list of parsed award criteria
     */
    private static List<ParsedAwardCriterion> parseAwardCriteria(final Document form) {
        final List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();
        for (int i = 1; i <= 10; i++) {
            final String criterionName = VestnikTenderParserUtils.getFieldValue(form,
                    String.format("FormItems\\.Kriteria%d_IV_2_1", i));
            if (criterionName != null && !criterionName.isEmpty()) {
                awardCriteria.add(new ParsedAwardCriterion().setName(criterionName)
                        .setWeight(VestnikTenderParserUtils.getFieldValue(form,
                                String.format("FormItems\\.Vaha%d_IV_2_1", i))));
            }
        }
        return awardCriteria;
    }

    /**
     * Parses information about EU funding.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return funding with details or null if information about EU funds not found
     */
    private static ParsedFunding parseEuFunding(final Document form) {
        final String isEuFund = VestnikTenderParserUtils.getCheckedInputValue(form,
                "FormItems\\.((Financovani(ZProstredku)?Spolecenstvi_VI_(1|2))|(ZakazkaSeVztahujeKProjektu(A)" +
                        "?NeboProgramu((FinancivanemuZProstredkuSpolecenstvi_VI_1)|(_VI_2)|(ZEU_VI_1)))|" + "" + "" +
                        "(VerejnaSoutezSeVztahuje_VI_1))");
        if (isEuFund == null) {
            return null;
        }
        final ParsedFunding funding = new ParsedFunding().setIsEuFund(isEuFund);

        if (BooleanUtils.toBoolean(isEuFund)) {
            funding.setSource(VestnikTenderParserUtils.getTextAfterLabel(form,
                    "FormItems_(((PokudAno)?(Uvedte)?OdkazNaTytoProjekty((_VI_2)|(ANebo(Programy|Porgramy)_VI_1)))|"
                            + "(VerejnaSoutezSeVztahujeOdkaz_VI_2)|(FinancovaniSpolecenstvi_odkaz_VI_2))"));
        }
        return funding;
    }

    /**
     * Parses tender additional info.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return tender additional info
     */
    private static String parseAdditionalInfo(final Document form) {
        return VestnikTenderParserUtils.getElementContent(
                form.select("textarea[name~=FormItems\\.DalsiInfo(rmace)?_VI_.]").first());
    }

    /**
     * Gets form source ID (form evidence number in Vestnik).
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return form source ID (called form evidence number in Vestnik)
     */
    private static String getFormId(final Document form) {
        return VestnikTenderParserUtils.getElementContent(
                form.select("div.iform-header > a[title=Seznam souvisejících formulářů]").first());
    }

    @Override
    public String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "CZ";
    }

    @Override
    protected List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
