package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getCheckedInputValue;
import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getFieldValue;
import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getFormSubsectionByName;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Handler for European forms.
 */
abstract class VVZEuFormsHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZEuFormsHandler.class);

    // =================================
    // SECTION I
    // =================================

    // ---------------------------------
    // SUBSECTION I.1)
    // ---------------------------------

    /**
     * Parses all the buyers.
     *
     * @param sectionI
     *         section I html
     *
     * @return list of parsed buyers
     */
    static List<ParsedBody> parseBuyers(final Element sectionI) {
        final List<ParsedBody> buyers = new ArrayList<>();
        final Elements buyersHtmls = sectionI.select("div[model~=.*AddressContractingBody\\[\\d+\\]$]");
        for (Element buyerHtml : buyersHtmls) {
            final ParsedBody buyer = VVZTenderParser.parseBody(buyerHtml);
            buyers.add(buyer);
        }

        // set first buyer as leader if there are more buyers
        if (buyers.size() > 1) {
            buyers.get(0).setIsLeader(Boolean.TRUE.toString());
        }

        return buyers;
    }

    // ---------------------------------
    // SUBSECTION I.2)
    // ---------------------------------

    /**
     * Parses whether the tender is central procurement.
     *
     * @param root
     *         html segment for parsing
     *
     * @return true if the tender is central procurement
     */
    static String parseCentralProcurement(final Element root) {
        return BooleanUtils.toStringTrueFalse(
                VVZTenderParserUtils.isInputFieldChecked(root, "Body\\.CentralPurchasing"));
    }

    /**
     * Parses whether the tender is joint procurement.
     *
     * @param root
     *         html segment for parsing
     *
     * @return true if the tender is joint procurement
     */
    static String parseJointProcurement(final Element root) {
        return BooleanUtils.toStringTrueFalse(
                VVZTenderParserUtils.isInputFieldChecked(root, "Body\\.JointProcurementInvolved"));
    }

    // ---------------------------------
    // SUBSECTION I.4)
    // ---------------------------------

    /**
     * Returns the type of buyer (eg. ministry, national agency etc.)
     *
     * @param sectionI
     *         root element for parsing buyer type
     *
     * @return type of the buyer
     */
    static String parseBuyerType(final Element sectionI) {
        String buyerType = VVZTenderParserUtils.getCheckedInputValue(sectionI, "Body\\.CAType$");

        // if "OTHER" is checked, parse also the specification of "OTHER"
        if (buyerType != null && buyerType.equalsIgnoreCase("OTHER")) {
            return VestnikTenderParserUtils.getFieldValue(sectionI, "Body\\.CATypeOther$");
        } else {
            return buyerType;
        }
    }

    // ---------------------------------
    // SUBSECTION I.5)
    // ---------------------------------

    /**
     * Returns the list of provided main activities of the buyer.
     *
     * @param sectionI
     *         root element for parsing buyer main activities
     *
     * @return list of buyer main activities
     */
    static String parseBuyerMainActivity(final Element sectionI) {
        String mainActivity = VVZTenderParserUtils.getCheckedInputValue(sectionI, "Body\\.CAActivity$");

        // if "OTHER" is checked, parse also the specification of "OTHER"
        if (mainActivity != null && mainActivity.equalsIgnoreCase("OTHER")) {
            return VestnikTenderParserUtils.getFieldValue(sectionI, "Body\\.CAActivityOther$");
        } else {
            return mainActivity;
        }
    }

    // ---------------------------------
    // SUBSECTION I.6)
    // ---------------------------------

    /**
     * Returns the list of provided main activities of the buyer.
     *
     * @param sectionI
     *         root element for parsing buyer main activities
     *
     * @return list of buyer main activities
     */
    static String parseBuyerMainActivityFromI6(final Element sectionI) {
        String mainActivity = VVZTenderParserUtils.getCheckedInputValue(sectionI, "Body\\.CEActivity$");

        // if "OTHER" is checked, parse also the specification of "OTHER"
        if (mainActivity != null && mainActivity.equalsIgnoreCase("OTHER")) {
            return VestnikTenderParserUtils.getFieldValue(sectionI, "Body\\.CEActivityOther$");
        } else {
            return mainActivity;
        }
    }

    // =================================
    // SECTION II
    // =================================

    // ---------------------------------
    // SUBSECTION II.1.2)
    // ---------------------------------

    /**
     * Parses tender CPVs.
     *
     * @param sectionII
     *         section II html
     *
     * @return parsed CPVs
     */
    static List<ParsedCPV> parseTenderCPVCodes(final Element sectionII) {
        final Element subsectionII1 = getFormSubsectionByName(sectionII, "II\\.1\\).*");
        return VVZTenderParser.parseCPVCodes(subsectionII1);
    }

    // ---------------------------------
    // SUBSECTION II.1.4)
    // ---------------------------------

    /**
     * Parses tender description.
     *
     * @param sectionII
     *         section II html
     *
     * @return tender description
     */
    static String parseTenderDescription(final Element sectionII) {
        return VVZTenderParserUtils.getFieldValue(sectionII, ".*\\.ShortDescr$");
    }

    // ---------------------------------
    // SUBSECTION II.2)
    // ---------------------------------

    /**
     * Gets html fragments for lots.
     *
     * @param form
     *         form to be parsed
     *
     * @return lots html fragments
     */
    static Elements getLotsHtmls(final Document form) {
        final Element sectionII = VVZTenderParser.getSectionII(form);
        return sectionII.select("div[model~=.*ObjectDescrList\\[\\d+\\]$]");
    }

    // ---------------------------------
    // SUBSECTION II.2.3) and VII.I.3
    // ---------------------------------

    /**
     * Parses address of implementation (NUTS and raw address).
     *
     * @param addressHtml
     *         html with address info
     *
     * @return address of implementation
     */
    static ParsedAddress parseAddressOfImplementation(final Element addressHtml) {
        final List<String> nuts = new ArrayList<>();

        nuts.addAll(addressHtml.select("input[name~=.*\\.NutsList\\[\\d+\\]$]")
                .stream()
                .filter(nutsCode -> !nutsCode.attr("value").isEmpty())
                .map(nutsCode -> nutsCode.attr("value"))
                .collect(Collectors.toList()));

        return new ParsedAddress().setRawAddress(getFieldValue(addressHtml, ".*MainSite")).setNuts(nuts);
    }

    // ---------------------------------
    // SUBSECTION II.2.4)
    // ---------------------------------

    /**
     * Parses tender description from subsection II.2.4.
     *
     * @param sectionII
     *         section II html
     *
     * @return tender description
     */
    static String parseTenderDescriptionFromII24(final Element sectionII) {
        return VVZTenderParserUtils.getFieldValue(sectionII, ".*ShortDesc$");
    }

    // ---------------------------------
    // SUBSECTION II.2.5)
    // ---------------------------------

    /**
     * Parses lot award criteria.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot award criteria
     */
    static List<ParsedAwardCriterion> parseLotAwardCriteria(final Element lotHtml) {
        List<ParsedAwardCriterion> parsedCriteria = new ArrayList<>();

        // parse quality criteria
        for (Element qualityCriterion : lotHtml.select("div[model~=.*ACQualityList\\[\\d+\\]]")) {
            final ParsedAwardCriterion parsedQualityCriterion = parseCriterion(qualityCriterion, false);
            if (parsedQualityCriterion != null) {
                parsedCriteria.add(parsedQualityCriterion);
            }
        }

        // parse cost criteria
        for (Element costCriterion : lotHtml.select("div[model~=.*ACCostList\\[\\d+\\]]")) {
            final ParsedAwardCriterion parsedCostCriterion = parseCriterion(costCriterion, true);
            if (parsedCostCriterion != null) {
                parsedCriteria.add(parsedCostCriterion);
            }
        }

        // parse price criterion
        final String isPriceCriterionChecked = VVZTenderParserUtils.getCheckedInputValue(lotHtml, ".*IsCostOrPrice");
        if (isPriceCriterionChecked != null && (isPriceCriterionChecked.equalsIgnoreCase(
                "true") || isPriceCriterionChecked.equalsIgnoreCase("PRICE"))) {
            parsedCriteria.add(new ParsedAwardCriterion().setIsPriceRelated(Boolean.TRUE.toString())
                    .setWeight(getFieldValue(lotHtml, ".*ACPrice\\.Weightning"))
                    .setName("PRICE"));
        }
        return parsedCriteria;
    }

    /**
     * Parses name and weight of award criterion.
     *
     * @param criterionDiv
     *         html segment with criterion info
     * @param isPriceRelated
     *         boolean whether the criterion is price related or not (null if unknown)
     *
     * @return parsed criterion
     */
    static ParsedAwardCriterion parseCriterion(final Element criterionDiv, final Boolean isPriceRelated) {
        final String name = getFieldValue(criterionDiv, ".*Criterion$");
        if (StringUtils.isNotEmpty(name)) {
            return new ParsedAwardCriterion().setName(name)
                    .setWeight(getFieldValue(criterionDiv, ".*Weightning$"))
                    .setIsPriceRelated(BooleanUtils.toStringTrueFalse(isPriceRelated));
        }
        return null;
    }

    // ---------------------------------
    // SUBSECTION II.2.7)
    // ---------------------------------

    /**
     * Parses lot estimated duration in months.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot estimated duration in months
     */
    static String parseLotEstimatedDurationInMonths(final Element lotHtml) {
        return getFieldValue(lotHtml, ".*TimeFrameDurationMonths");
    }

    /**
     * Parses lot estimated duration in days.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot estimated duration in days
     */
    static String parseLotEstimatedDurationInDays(final Element lotHtml) {
        return getFieldValue(lotHtml, ".*TimeFrameDurationDays");
    }

    /**
     * Parses lot estimated start date.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot estimated start date
     */
    static String parseLotEstimatedStartDate(final Element lotHtml) {
        return getFieldValue(lotHtml, ".*TimeFrameFrom");
    }

    /**
     * Parses lot estimated completion date.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot estimated completion date
     */
    static String parseLotEstimatedCompletionDate(final Element lotHtml) {
        return getFieldValue(lotHtml, ".*TimeFrameTo");
    }

    // ---------------------------------
    // SUBSECTION II.2.11)
    // ---------------------------------

    /**
     * Parses whether the lot has options or not.
     *
     * @param lotHtml
     *         lot html
     *
     * @return true if lot has options
     */
    static String parseHasLotOptionsFromCheckbox(final Element lotHtml) {
        return BooleanUtils.toStringTrueFalse(VVZTenderParserUtils.isInputFieldChecked(lotHtml, ".*Options"));
    }

    /**
     * Parses whether the lot has options or not.
     *
     * @param lotHtml
     *         lot html
     *
     * @return true if lot has options
     */
    static String parseHasLotOptionsFromRadio(final Element lotHtml) {
        return VVZTenderParserUtils.getCheckedInputValue(lotHtml, ".*Options");
    }

    // =================================
    // SECTION IV
    // =================================

    // ---------------------------------
    // SUBSECTION IV.1.1) (CZ III.1.1)
    // ---------------------------------

    /**
     * Parses info about accelerated procedure.
     *
     * @param sectionIV
     *         section IV html
     * @param tender
     *         tender to be updated
     *
     * @return tender updated with info about accelerated procedure
     */
    static ParsedTender parseAcceleratedProcedureTypeInfo(final Element sectionIV, final ParsedTender tender) {
        final Boolean isAcceleratedProcedure = parseIsAcceleratedProcedure(sectionIV);
        tender.setIsAcceleratedProcedure(BooleanUtils.toStringTrueFalse(isAcceleratedProcedure));

        if (BooleanUtils.toBoolean(isAcceleratedProcedure)) {
            tender.setAcceleratedProcedureJustification(parseAcceleratedProcedureJustification(sectionIV));
        }
        return tender;
    }

    /**
     * Parses whether the procedure is accelerated.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return true if the procedure is accelerated
     */
    private static Boolean parseIsAcceleratedProcedure(final Element sectionIV) {
        return VVZTenderParserUtils.isInputFieldChecked(sectionIV,
                ".*\\.(IsOpenAccelerated|IsCompetitiveNegotiationAccelerated|IsRestrictedAccelerated)$");
    }

    /**
     * Parses accelerated procedure justification text.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return accelerated procedure justification
     */
    private static String parseAcceleratedProcedureJustification(final Element sectionIV) {
        return VVZTenderParserUtils.getFieldValue(sectionIV, ".*\\.Accelerated$");
    }

    // ---------------------------------
    // SUBSECTION IV.1.3)
    // ---------------------------------

    /**
     * Parses whether the tender is framework agreement.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return true if tender is framework agreement
     */
    static String parseIsFrameworkAgreement(final Element sectionIV) {
        return BooleanUtils.toStringTrueFalse(VVZTenderParserUtils.isInputFieldChecked(sectionIV, ".*\\.IsFramework$"));
    }

    /**
     * Parses whether the tender is DPS.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return true if tender is DPS
     */
    static String parseIsDPS(final Element sectionIV) {
        return BooleanUtils.toStringTrueFalse(VVZTenderParserUtils.isInputFieldChecked(sectionIV, ".*\\.DPS$"));
    }

    // ---------------------------------
    // SUBSECTION IV.1.8)
    // ---------------------------------

    /**
     * Parses whether the tender is covered by GPA.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return true if tender is covered by GPA
     */
    static String parseIsCoveredByGPA(final Element sectionIV) {
        return getCheckedInputValue(sectionIV, ".*\\.GPA");
    }

    // ---------------------------------
    // SUBSECTION IV.1.9)
    // ---------------------------------

    /**
     * Parses eligibility criteria.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return eligibility criteria
     */
    static String parseEligibilityCriteria(final Element sectionIV) {
        return VVZTenderParserUtils.getFieldValue(sectionIV, ".*CriteriaEvaluation$");
    }

    // ---------------------------------
    // SUBSECTION IV.2.1)
    // ---------------------------------

    /**
     * Parses previous publication in TED.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return pervious publication in TED
     */
    static ParsedPublication parsePreviousTedPublication(final Element sectionIV) {
        final String formId = VVZTenderParserUtils.getFieldValue(sectionIV, ".*\\.NoticeNumber$");
        if (StringUtils.isNotEmpty(formId)) {
            return new ParsedPublication().setIsIncluded(false)
                    .setSource(VVZTenderParser.TED_SOURCE_URL)
                    .setSourceId(formId);
        }
        return null;
    }

    // =================================
    // SECTION VI
    // =================================

    // ---------------------------------
    // SUBSECTION VI.4.1)
    // ---------------------------------

    /**
     * Parses name of the appeal body.
     *
     * @param sectionVI
     *         section VI html
     *
     * @return appeal body name
     */
    static String parseAppealBodyName(final Element sectionVI) {
        return getFieldValue(sectionVI, ".*\\.AddressReviewBody\\.OfficalName");
    }

    // ---------------------------------
    // SUBSECTION VI.4.2)
    // ---------------------------------

    /**
     * Parses name of the mediation body.
     *
     * @param sectionVI
     *         section VI
     *
     * @return mediation body name
     */
    static String parseMediationBodyName(final Element sectionVI) {
        return getFieldValue(sectionVI, ".*\\.AddressMediationBody\\.OfficalName");
    }
}
