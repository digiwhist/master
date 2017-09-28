package eu.digiwhist.worker.bg.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.TenderSize;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static eu.digiwhist.dataaccess.dto.codetables.PublicationSources.BG_AOP;
import static eu.digiwhist.worker.bg.parsed.AOPParserUtils.getAnythingUnderLabel;
import static eu.digiwhist.worker.bg.parsed.AOPParserUtils.getTableUnderTitle;
import static eu.digiwhist.worker.bg.parsed.AOPParserUtils.getValueByLabel;

/**
 * Created by michalriha on 12/07/2017.
 */
public final class AOPTenderFirstFormHandler {

    /**
     * Supress default constructor for noninstatiability.
     */
    private AOPTenderFirstFormHandler() {
    }

    /**
     * Parses First form type data.
     *
     * @param doc    parsed document
     * @param sourceFormType    source form type
     * @param url    url
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final Document doc, final String sourceFormType, final String url) {
        Element sectionAI = AOPParserUtils.getSectionAI(doc);
        Element sectionI = AOPParserUtils.getSectionI(doc);
        Element sectionII = AOPParserUtils.getSectionII(doc);
        Element sectionIII = AOPParserUtils.getSectionIII(doc);
        Element sectionIV = AOPParserUtils.getSectionIV(doc);

        ParsedTender parsedTender = new ParsedTender();

        ParsedBody buyer = new ParsedBody()
                .setName(AOPParserUtils.getValueByLabel("I_1_officialName", sectionI))
                .addBodyId(new BodyIdentifier()
                        .setId(AOPParserUtils.getValueByLabel("I_1_nationalID", sectionI))
                        .setScope(BodyIdentifier.Scope.BG)
                        .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                .setContactName(AOPParserUtils.getValueByLabel("I_1_contact_point", sectionI))
                .setPhone(AOPParserUtils.getValueByLabel("I_1_phone", sectionI))
                .setAddress(new ParsedAddress()
                        .setStreet(AOPParserUtils.getValueByLabel("I_1_address", sectionI))
                        .setCity(AOPParserUtils.getValueByLabel("I_1_town", sectionI))
                        .setPostcode(AOPParserUtils.getValueByLabel("I_1_postal_code", sectionI))
                        .setCountry(AOPParserUtils.getValueByLabel("I_1_country", sectionI))
                        .setUrl(AOPParserUtils.getAnythingUnderLabel("I_1_url_general", sectionI))
                        .addNuts(AOPParserUtils.getValueByLabel("I_1_nuts", sectionI)))
                .setEmail(AOPParserUtils.getValueByLabel("I_1_e_mail", sectionI))
                .setBuyerType(AOPParserUtils.getTableUnderTitle("I.4\\)", doc))
                .addMainActivity(AOPParserUtils.getTableUnderTitle("Основна дейност", doc));

        parsedTender
                .setBuyerAssignedId(AOPParserUtils.getValueByLabel("AI_case_num", sectionAI))
                .setSize(!AOPParserUtils.isEnabled("AI_publish", sectionAI)
                        ? TenderSize.BELOW_THE_THRESHOLD.name() : null)
                .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setSource(BG_AOP)
                        .setSourceFormType(sourceFormType)
                        .setPublicationDate(AOPParserUtils.getValueByLabel("AI_out_date", sectionAI))
                        .setDispatchDate(JsoupUtils.selectText("span:containsOwn(VI.5\\)) + span.input_value",
                                doc))
                        .setHumanReadableUrl(url))
                .addBuyer(buyer)
                .setIsJointProcurement(AOPParserUtils.isEnabled("I_2_joint_procurement_involved", sectionI).toString())
                .setIsCentralProcurement(AOPParserUtils.isEnabled("I_2_central_purchasing", sectionI).toString())
                .setTitle(AOPParserUtils.getValueByLabel("II_1_1_title__0", sectionII))
                .setCpvs(parseCPVs(sectionII))
                .setSupplyType(AOPParserUtils.getValueByLabel("II_1_3_type_contract__0", sectionII))
                .setDescription(AOPParserUtils.getValueByLabel("II_1_4_short_descr__0", sectionII))
                .setHasLots(AOPParserUtils.isEnabled("II_1_6_has_lots__0", sectionII).toString())
                .setEstimatedDurationInDays(getTableUnderTitle("II_2_7_time_frame_duration_d__0__0", sectionII))
                .setPersonalRequirements(getValueByLabel("III_1_1_list_of_descriptions", sectionIII))
                .setDeposits(getValueByLabel("III_1_6_deposit_guarantee_required", sectionIII))
                .setEconomicRequirements(getValueByLabel("III_1_2_has_economic_criteria", sectionIII))
                .setTechnicalRequirements(getValueByLabel("III_1_3_has_technical_criteria", sectionIII))
                .setNationalProcedureType(getAnythingUnderLabel("IV_1_1_pt_radio", sectionIV))
                .setIsAcceleratedProcedure(getValueByLabel("IV_1_1_accelerated_procedure", sectionIV))
                .setAcceleratedProcedureJustification(getValueByLabel("IV_1_1_justification", sectionIV))
                .setIsFrameworkAgreement(getValueByLabel("IV_1_3_fi_radio", sectionIV))
                .setMaxFrameworkAgreementParticipants(getValueByLabel("IV_1_3_nb_participants", sectionIV))
                .setIsDps(getValueByLabel("IV_1_3_has_dps", sectionIV))
                .setIsElectronicAuction(getValueByLabel("IV_1_6_has_dps_additional", sectionIV))
                .setIsCoveredByGpa(getValueByLabel("IV_1_8_gpa", sectionIV))
                .addPublication(new ParsedPublication()
                        .setSourceTenderId(getValueByLabel("IV_2_1_notice_number_oj", sectionIV))
                        .setBuyerAssignedId(getAnythingUnderLabel("IV_2_1_notice_number_oj", sectionIV)))
                .addPublication(new ParsedPublication()
                        .setSourceTenderId(getValueByLabel("IV_2_1_aop_notice_number", sectionIV))
                        .setBuyerAssignedId(getValueByLabel("IV_2_1_aop_notice_number", sectionIV)))
                .setBidDeadline(getValueByLabel("IV_2_2_date_receipt_tenders", doc))
                .addEligibleBidLanguage(JsoupUtils.selectText("b:containsOwn(IV.2.4\\)) ~ span.input_value",
                        doc))
                .setAwardDeadline(getValueByLabel("IV_2_6_date_tander_valid", doc))
                .setAwardDeadlineDuration(getValueByLabel("IV_2_6_duration_tander_valid", doc))
                .setIsEInvoiceAccepted(getValueByLabel("VI_2_einvoicing", doc))
                .setAdditionalInfo(getTableUnderTitle("VI.3\\)", doc))
                .setAppealBodyName(getValueByLabel("VI_4_1_officialName", doc));


        if (sourceFormType != null) {
            if (sourceFormType.contains("Обявление за поръчка")) {
                parsedTender = AOPContractNoticeFirstFormHandler.parse(parsedTender, doc);
            } else if (sourceFormType.contains("Обявление за възложена поръчка")) {
                parsedTender = AOPContractAwardFirstFormHandler.parse(parsedTender, doc);
            } else {
                LoggerFactory.getLogger(AOPTenderFirstFormHandler.class.getClass().getName())
                        .warn("Unknown form type {}, default handler was used", sourceFormType);
            }
        }

        return parsedTender;
    }

    /**
     * Parses list of CPVs.
     *
     * @param context context that includes CPVs data
     *
     * @return non-empty list of CPVs or null
     */
    private static List<ParsedCPV> parseCPVs(final Element context) {
        List<ParsedCPV> cpvs = new ArrayList<>();

        String mainCPV = AOPParserUtils.getValueByLabel("II_1_2___0mcpv", context);
        if (mainCPV != null) {
            cpvs.add(new ParsedCPV()
                    .setCode(mainCPV)
                    .setIsMain(Boolean.TRUE.toString()));
        }

        String additionalCpvs = AOPParserUtils.getValueByLabel("II_1_2___0scpv", context);
        if (additionalCpvs != null && !additionalCpvs.isEmpty()) {
            Arrays.stream(additionalCpvs.split(",")).forEach(c -> {
                cpvs.add(new ParsedCPV()
                        .setCode(c)
                        .setIsMain(Boolean.FALSE.toString()));
            });
        }

        return cpvs.isEmpty() ? null : cpvs;
    }
}
