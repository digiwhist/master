package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for Contract Award Notice on Vestnik.
 */
abstract class VestnikContractAwardHandler {
    private static final Logger logger = LoggerFactory.getLogger(VestnikContractAwardHandler.class);

    /**
     * Parses Contract Award Notice specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page
     *
     * @return updated tender object with data parsed from Contract Award Notice form
     */
    static ParsedTender parseCommonContractAwardAttributes(final ParsedTender tender, final Document form) {
        // buyer ICO from form header
        final String bodyIdFromHeader = VestnikTenderParserUtils.getBodyIdFromFormHeader(form, "zadavatele");
        if (bodyIdFromHeader != null) {
            if (tender.getBuyers().get(0) != null) {
                tender.getBuyers()
                        .get(0)
                        .addBodyId(new BodyIdentifier().setId(bodyIdFromHeader)
                                .setType(BodyIdentifier.Type.HEADER_ICO)
                                .setScope(BodyIdentifier.Scope.CZ));
            }
        }

        // FINAL PRICE
        tender.setFinalPrice(parseFinalPrice(form));

        final Element attachmentD = form.select(
                "form:has(div.iform-section:contains(Odůvodnění zadání zakázky bez předchozího zveřejnění " +
                        "oznámení o zakázce v)) > fieldset.iform-content")
                .first();
        if (attachmentD != null) {
            for (final Element checkedInput : attachmentD.select("div.iform-field > input[checked]")) {
                tender.addNpwpReason(checkedInput.attr("name"));
            }
        }
        return tender;
    }

    /**
     * Parses total final price of tender.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return final price with available details (VAT, currency etc.)
     */
    private static ParsedPrice parseFinalPrice(final Document form) {
        final Element subsectionII2 = VestnikTenderParserUtils.getFormSubsectionByName(form, "II\\.2\\)");
        if (subsectionII2 == null) {
            logger.warn("Unable to find section II.2 with final price info. No final price will be parsed.");
            return null;
        }

        final ParsedPrice finalPrice = new ParsedPrice().setCurrency(
                VestnikTenderParserUtils.getSelectedOptionValue(subsectionII2, "FormItems\\.Mena(1|2)?_II_2_1"));

        final String isVatIncluded = VestnikTenderParserUtils.getCheckedInputValue(subsectionII2,
                "FormItems\\.Dph(1|2)_II_2_1");

        final String amount = VestnikTenderParserUtils.getFieldValue(subsectionII2, "FormItems\\.Hodnota_II_2_1");
        final String minAmount = VestnikTenderParserUtils.getFieldValue(subsectionII2,
                "FormItems\\.NejnizsiNabidka_II_2_1");
        final String maxAmount = VestnikTenderParserUtils.getFieldValue(subsectionII2,
                "FormItems\\.NejvyssiNabidka_II_2_1");

        if (Boolean.valueOf(isVatIncluded)) {
            finalPrice.setAmountWithVat(amount).setMinAmountWithVat(minAmount).setMaxAmountWithVat(maxAmount);
            String vatSelector;
            if (amount != null && !amount.isEmpty()) {
                vatSelector = "FormItems\\.SazbaDph1_II_2_1";
            } else {
                vatSelector = "FormItems\\.SazbaDph2_II_2_1";
            }
            finalPrice.setVat(VestnikTenderParserUtils.getFieldValue(subsectionII2, vatSelector));
        } else {
            finalPrice.setNetAmount(amount).setMinNetAmount(minAmount).setMaxNetAmount(maxAmount);
        }
        return finalPrice;
    }

    /**
     * Parses all the lots from contract award notice.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    static List<ParsedTenderLot> parseLots(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();
        final String supplierIco = getSupplierIco(form);
        Elements lotsSections = getLotsSections(form);
        for (int i = 0; i < lotsSections.size(); i++) {
            Element lotSection = lotsSections.get(i);
            ParsedTenderLot lot = parseLot(lotSection, i + 1);
            // parse winning bid
            ParsedBid winningBid = parseWinningBid(lotSection, supplierIco);
            winningBid.setPrice(parseFinalLotPrice(lotSection));
            lot.addBid(winningBid);

            lots.add(lot);
        }
        return lots;
    }

    /**
     * Parses supplier ICO from the form header.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return supplier ICO
     */
    static String getSupplierIco(final Document form) {
        return VestnikTenderParserUtils.getBodyIdFromFormHeader(form, "dodavatele");
    }

    /**
     * Parses lots sections from the form.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return sections for all the lots
     */
    static Elements getLotsSections(final Document form) {
        return VestnikTenderParserUtils.getFormSections(form,
                ".*((Zadání( a hodnota)? zakázky)|(Výsledky veřejné soutěže))");
    }

    /**
     * Parses tender lot from given lot form (HTML fragment).
     *
     * @param lotSection
     *         parsed tender lot section
     * @param lotPosition
     *         order in which the lot appears on the source page
     *
     * @return parsed tender lot
     */
    static ParsedTenderLot parseLot(final Element lotSection, final int lotPosition) {
        return new ParsedTenderLot().setPositionOnPage(Integer.toString(lotPosition))
                .setLotNumber(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.(((ZakazkaCast)|(CastZakazkyC))_V(_(1|0))?)|(C_V_1)"))
                .setContractNumber(
                        VestnikTenderParserUtils.getFieldValue(lotSection, "AttModels.*AttItems\\.ZakazkaC_V(_(1|0))?"))
                .setTitle(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.(Zakazka)?Nazev_V(_(1|0))?"))
                .setAwardDecisionDate(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.Datum(ZadaniZakazky)?_V_1(_1)?"))
                .setBidsCount(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.(Poce(t)?(Obdrzenych)?Nabidek_V_(1_)?2)|(PocetUcastniku_V_1_1)"))
                .setElectronicBidsCount(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.Poce(t)?Nabidek(El|ObdrzenychElektronickouCestou)_V_(1_)?2"))
                .setForeignCompaniesBidsCount(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.PocetZahranicnichUcastniku_V_1_2"))
                .setEstimatedPrice(parseEstimatedLotPrice(lotSection));
    }

    /**
     * Parses tender lot estimated price.
     *
     * @param lotSection
     *         parsed tender lot section
     *
     * @return lot estimated price
     */
    private static ParsedPrice parseEstimatedLotPrice(final Element lotSection) {
        final ParsedPrice estimatedLotPrice = new ParsedPrice().setCurrency(
                VestnikTenderParserUtils.getSelectedOptionValue(lotSection, "AttModels.*AttItems\\.Mena1_V_(1_)?4"));

        final String isVatIncluded = VestnikTenderParserUtils.getCheckedInputValue(lotSection,
                "AttModels.*AttItems\\.Dph1_V_(1_)?4");

        final String amount = VestnikTenderParserUtils.getFieldValue(lotSection,
                "AttModels.*AttItems\\.Hodnota1_V_(1_)?4");

        if (Boolean.valueOf(isVatIncluded)) {
            estimatedLotPrice.setAmountWithVat(amount)
                    .setVat(VestnikTenderParserUtils.getFieldValue(lotSection,
                            "AttModels.*AttItems\\.SazbaDph1_V_(1_)?4"));
        } else {
            estimatedLotPrice.setNetAmount(amount);
        }
        return estimatedLotPrice;
    }

    /**
     * Parses winning bid with information about price, supplier, etc.
     *
     * @param lotSection
     *         parsed tender lot section
     * @param supplierIco
     *         supplier ICO from header
     *
     * @return winning bid
     */
    static ParsedBid parseWinningBid(final Element lotSection, final String supplierIco) {
        final ParsedBid winningBid = new ParsedBid().setIsWinning(Boolean.TRUE.toString())
                .setAnnualPriceYearsCount(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.((RocniHodnota)|(PocetRoku))_V_(1_)?4"))
                .setMonthlyPriceMonthsCount(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.((MesicniHodnota)|(NeboPocetMesicu))_V_(1_)?4"))
                .setIsSubcontracted(VestnikTenderParserUtils.getCheckedInputValue(lotSection,
                        "AttModels.*AttItems\\.(JePravdepodobneZeZakazkaBudeProvedena)?Subdodavatelsky_V_(1_)?5"))
                .setSubcontractedProportion(VestnikTenderParserUtils.getFieldValue(lotSection,
                        "AttModels.*AttItems\\.Pomer(Sub)?_V_(1_)?5"))
                .setSubcontractedValue(parseSubcontractedValue(lotSection));

        final Element supplierSection = getSupplierSection(lotSection);
        final ParsedBody supplier = VestnikTenderParserUtils.parseBody(supplierSection);
        // supplier ICO from form header
        if (supplierIco != null) {
            supplier.addBodyId(new BodyIdentifier().setId(supplierIco)
                    .setScope(BodyIdentifier.Scope.CZ)
                    .setType(BodyIdentifier.Type.HEADER_ICO));
        }
        winningBid.addBidder(supplier);
        return winningBid;
    }

    /**
     * Parses lot subcontracted value.
     *
     * @param lotSection
     *         parsed tender lot section
     *
     * @return subcontracted value
     */
    private static ParsedPrice parseSubcontractedValue(final Element lotSection) {
        String netAmount = VestnikTenderParserUtils.getFieldValue(lotSection,
                "AttModels.*AttItems\\.Hodnota((Sub)|(BezDPH))_V_(1_)?5");
        if (StringUtils.isNotEmpty(netAmount)) {
            return new ParsedPrice().setNetAmount(netAmount)
                    .setCurrency(VestnikTenderParserUtils.getSelectedOptionValue(lotSection,
                            "AttModels.*AttItems\\.Mena(Subdodavky)?_V_(1_)?5"));
        }
        return null;
    }

    /**
     * Parses lot final price (price of winning bid) or lowest and highest offer.
     *
     * @param lotSection
     *         parsed tender lot section
     *
     * @return price of winning bid (lot final price) or lowest and highest offer.
     */
    static ParsedPrice parseFinalLotPrice(final Element lotSection) {
        final ParsedPrice finalLotPrice = new ParsedPrice().setCurrency(
                VestnikTenderParserUtils.getSelectedOptionValue(lotSection,
                        "AttModels.*AttItems\\.Mena(2|3)_V_(1_)?4"));

        final String isVatIncluded = VestnikTenderParserUtils.getCheckedInputValue(lotSection,
                "AttModels.*AttItems\\.Dph(2|3)_V_(1_)?4");

        final String amount = VestnikTenderParserUtils.getFieldValue(lotSection,
                "AttModels.*AttItems.Hodnota2_V_(1_)?4");
        final String minAmount = VestnikTenderParserUtils.getFieldValue(lotSection,
                "AttModels.*AttItems\\.NejnizsiNabidka_V_(1_)?4");
        final String maxAmount = VestnikTenderParserUtils.getFieldValue(lotSection,
                "AttModels.*AttItems\\.NejvyssiNabidka_V_(1_)?4");

        if (Boolean.valueOf(isVatIncluded)) {
            finalLotPrice.setAmountWithVat(amount).setMinAmountWithVat(minAmount).setMaxAmountWithVat(maxAmount);
            String vatSelector;
            if (amount != null && !amount.isEmpty()) {
                vatSelector = "AttModels.*AttItems\\.SazbaDph2_V_(1_)?4";
            } else {
                vatSelector = "AttModels.*AttItems\\.SazbaDph3_V_(1_)?4";
            }
            finalLotPrice.setVat(VestnikTenderParserUtils.getFieldValue(lotSection, vatSelector));
        } else {
            finalLotPrice.setNetAmount(amount).setMinNetAmount(minAmount).setMaxNetAmount(maxAmount);
        }
        return finalLotPrice;
    }

    /**
     * Gets HTML section with supplier info from lot section HTML.
     *
     * @param lotSection
     *         parsed tender lot section
     *
     * @return html fragment with supplier info
     */
    private static Element getSupplierSection(final Element lotSection) {
        final Elements lotFieldsets = lotSection.select("fieldset");
        if (lotFieldsets.size() > 1) {
            return lotFieldsets.get(1);
        }
        return lotSection;
    }
}
