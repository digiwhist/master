package eu.digiwhist.worker.bg.parsed;

import static eu.digiwhist.worker.bg.parsed.AOPParserUtils.getValueByLabel;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Contract notice handler.
 *
 * @author Tomas Mrazek
 */
public final class AOPContractNoticeFirstFormHandler {

    /**
     * Supress default constructor for noninstatiability.
     */
    private AOPContractNoticeFirstFormHandler() {
    }

    /**
     * Parses Contract notice specific data.
     *
     * @param tender parsed tender
     * @param doc    parsed document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender tender, final Document doc) {
        Element sectionI = AOPParserUtils.getSectionI(doc);
        Element sectionII = AOPParserUtils.getSectionII(doc);

        assert JsoupUtils.hasText("label[for=I_3_afi_radio] + span", sectionI,
                "Горепосоченото/ите място/места за контакт")
                : "unexpected further information provider";

        assert JsoupUtils.hasText("label[for=I_3_ap_radio] + span", sectionI,
                "Горепосоченото/ите място/места за контакт")
                : "unexpected bids recipient";

        ParsedBody buyer = tender.getBuyers().get(0);

        tender.setDocumentsLocation(new ParsedAddress()
                .setUrl(AOPParserUtils.getAnythingUnderLabel("I_3_url_document", sectionI)))
                .setFurtherInformationProvider(buyer)
                .setBidsRecipient(buyer)
                .setEstimatedPrice(AOPParserUtils.parseInlinePrice("II_1_5_val_estimated_total__0_", sectionII));


        Element lotsNode = JsoupUtils.selectFirst("p:contains(II.2\\) Описание) + table", sectionII);

        tender.addLot(new ParsedTenderLot()
                .setPositionOnPage("1")
                .addCpv(new ParsedCPV()
                        .setCode(getValueByLabel("II_2_2_mcpv__0__0__0", lotsNode))
                        .setIsMain(Boolean.TRUE.toString()))
                .setAddressOfImplementation(new ParsedAddress()
                        .addNuts(getValueByLabel("II_2_3_nuts__0__0__0", lotsNode))
                        .setRawAddress(getValueByLabel("II_2_3_main_site__0__0", lotsNode)))
                .setDescription(getValueByLabel("II_2_4_short_descr__0__0", lotsNode))
                .addAwardCriterion(new ParsedAwardCriterion()
                        .setName(JsoupUtils.selectText("tr[valign=top]:has" +
                                "(label[for=II_2_5_award_criteria_docaward_criteriahas_ac_quality__0__0]) + tr > td",
                                lotsNode))
                        .setWeight(JsoupUtils.selectText("tr[valign=top]:has(label[for=II_2_5_award_criteri" +
                                "a_docaward_criteriahas_ac_quality__0__0]) + tr > td + td sup", lotsNode)))
                .setEstimatedPrice(new ParsedPrice()
                        .setNetAmount(getValueByLabel("II_2_6_val_object__0__0_cost", lotsNode))
                        .setCurrency(getValueByLabel("II_2_6_val_object__0__0_currency", lotsNode)))
                .setEstimatedStartDate(getValueByLabel("II_2_7_time_frame_date_start__0__0", lotsNode))
                .setEstimatedCompletionDate(getValueByLabel("II_2_7_time_frame_date_end__0__0", lotsNode))
                .setEnvisagedCandidatesCount(getValueByLabel("II_2_9_limit_candidate9_nb_envisaged_candida" +
                        "te__0__0", lotsNode))
                .setEnvisagedMinCandidatesCount(getValueByLabel("II_2_9_limit_candidate9_nb_min_limit_cand" +
                        "idate__0__0", lotsNode))
                .setEnvisagedMaxCandidatesCount(getValueByLabel("II_2_9_limit_candidate9_nb_max_limit_cand" +
                        "idate__0__0", lotsNode))
                .setLimitedCandidatesCountCriteria(getValueByLabel("II_2_9_limit_candidate9_criteria_candi" +
                        "date__0__0", lotsNode))
                .setAreVariantsAccepted(getValueByLabel("II_2_10_accepted_variants__0__0", lotsNode))
                .setHasOptions(getValueByLabel("II_2_11_options__0__0", lotsNode))
                .addFunding(new ParsedFunding()
                        .setIsEuFund(getValueByLabel("II_2_13_eu_union_funds_euf_radio__0__0", lotsNode))));




        return tender;
    }

}
