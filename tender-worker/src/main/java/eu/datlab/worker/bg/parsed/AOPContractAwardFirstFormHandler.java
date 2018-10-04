package eu.datlab.worker.bg.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;

import static eu.datlab.worker.bg.parsed.AOPParserUtils.getAnythingUnderLabel;
import static eu.datlab.worker.bg.parsed.AOPParserUtils.getTableUnderTitle;
import static eu.datlab.worker.bg.parsed.AOPParserUtils.getValueByLabel;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

/**
 * Created by michalriha on 19/06/2017.
 */
public final class AOPContractAwardFirstFormHandler {

    /**
     * Supress default constructor for noninstatiability.
     */
    private AOPContractAwardFirstFormHandler() {
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
        Element sectionII = AOPParserUtils.getSectionII(doc);
        Element sectionV = AOPParserUtils.getSectionV(doc);

        tender.addLot(new ParsedTenderLot()
                .setPositionOnPage("1")
                .setBidsCount(getValueByLabel("V_2_2_nb_tenders_nb_tenders_received__0", sectionV))
                .setSmeBidsCount(getValueByLabel("V_2_2_nb_tenders_nb_tenders_received_sme__0", sectionV))
                .setOtherEuMemberStatesCompaniesBidsCount(getValueByLabel("V_2_2_nb_tenders_nb_tenders" +
                        "_received_other_eu__0", sectionV))
                .setNonEuMemberStatesCompaniesBidsCount(getValueByLabel("V_2_2_nb_tenders_nb_tenders_r" +
                        "eceived_non_eu__0", sectionV))
                .setElectronicBidsCount(getValueByLabel("V_2_2_nb_tenders_nb_tenders_received_emeans__" +
                        "0", sectionV))
                .addBid(new ParsedBid()
                    .setIsConsortium(getValueByLabel("V_2_2_awarded_to_group__0", sectionV))
                        .addBidder(new ParsedBody()
                                .setName(getValueByLabel("V_2_3_address_contractorofficialName__0__0",
                                        sectionV))
                                .addBodyId(new BodyIdentifier()
                                        .setId(getValueByLabel("V_2_3_address_contractornationalID__0__0",
                                                sectionV))
                                        .setScope(BodyIdentifier.Scope.BG)
                                        .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                                .setAddress(new ParsedAddress()
                                        .setStreet(getValueByLabel("V_2_3_address_contractoraddress__0__0",
                                                sectionV))
                                        .setCity(getValueByLabel("V_2_3_address_contractortown__0__0",
                                                sectionV))
                                        .addNuts(getValueByLabel("V_2_3_address_contractornuts__0__0",
                                                sectionV))
                                        .setPostcode(getValueByLabel("V_2_3_address_contractorpostal_code__0__0",
                                                sectionV))
                                        .setCountry(getValueByLabel("V_2_3_address_contractorcountry__0__0",
                                                sectionV))
                                        .setUrl(getAnythingUnderLabel("V_2_3_address_contractorurl__0__0", sectionV)))
                                .setPhone(getValueByLabel("V_2_3_address_contractorphone__0__0", sectionV))
                                .setEmail(getValueByLabel("V_2_3_address_contractore_mail__0__0", sectionV))
                                .setIsSme(getValueByLabel("V_2_3_sme_radio__0__0", sectionV)))
                        .setPrice(new ParsedPrice()
                                .setNetAmount(getValueByLabel("V_2_4_award_contract_value_val_total__0_cost", sectionV))
                                .setCurrency(getValueByLabel("V_2_4_award_contract_value_val_total__0_currency",
                                        sectionV))
                                .setMinNetAmount(getValueByLabel("V_2_4_award_contract_value_val_range__0_low",
                                        sectionV))
                                .setMaxNetAmount(getValueByLabel("V_2_4_award_contract_value_val_range__0_high",
                                        sectionV)))
                        .setIsWinning(Boolean.TRUE.toString())
                        .setIsSubcontracted(getValueByLabel("V_2_5_likely_subcontracted__0", sectionV))
                        .setSubcontractedValue(new ParsedPrice()
                                .setNetAmount(getValueByLabel("V_2_5_subcontracting_val_subcontracting__0_cost",
                                        sectionV))
                                .setCurrency(getValueByLabel("V_2_5_subcontracting_val_subcontracting__0_currency",
                                        sectionV)))
                        .setSubcontractedProportion(getValueByLabel("V_2_5_subcontracting_pct_subcontracting__0",
                                sectionV)))
                .setEstimatedPrice(new ParsedPrice()
                        .setNetAmount(getValueByLabel("V_2_4_award_contract_value_val_estimated_total__0_cost",
                                sectionV))
                        .setCurrency(getValueByLabel("V_2_4_award_contract_value_val_estimated_total__0_currency",
                                sectionV)))



                .setContractNumber(getValueByLabel("V_0_contract_number_contract_no__0", sectionV))
                .addCpv(new ParsedCPV()
                        .setCode(getValueByLabel("II_2_2_mcpv__0__0__0", sectionV))
                        .setIsMain(Boolean.TRUE.toString()))
                .setAddressOfImplementation(new ParsedAddress()
                        .addNuts(getValueByLabel("II_2_3_nuts__0__0__0", sectionV))
                        .setRawAddress(getValueByLabel("II_2_3_main_site__0__0", sectionV)))
                .setDescription(getValueByLabel("II_2_4_short_descr__0__0", sectionV))
                .addFunding(new ParsedFunding()
                        .setIsEuFund(getValueByLabel("II_2_13_eu_union_funds_euf_radio__0__0", sectionV))));

        tender.setFinalPrice(new ParsedPrice()
                .setNetAmount(getValueByLabel("II_1_7_val_total__0_cost", sectionII))
                .setCurrency(getValueByLabel("II_1_7_val_total__0_currency", sectionII)))
                .setIsAwarded(getTableUnderTitle("V_0_aw_radio__0", sectionV));

        return tender;
    }
}
