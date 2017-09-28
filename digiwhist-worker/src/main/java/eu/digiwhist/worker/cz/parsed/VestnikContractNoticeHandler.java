package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Handler for Contract Notice on Vestnik.
 */
abstract class VestnikContractNoticeHandler {
    private static final Logger logger = LoggerFactory.getLogger(VestnikContractNoticeHandler.class);

    /**
     * Parses Contract Notice specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page
     *
     * @return updated tender object with data parsed from Contract Notice form
     */
    static ParsedTender parseCommonContractNoticeAttributes(final ParsedTender tender, final Document form) {
        tender.setBidDeadline(parseBidDeadline(form))
                .setEstimatedStartDate(
                        VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.(StanovaneDatum|Nebo)Zahajeni_II_3"))
                .setEstimatedCompletionDate(VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.Dokonceni_II_3"))
                .setEstimatedDurationInMonths(
                        VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.(DobaTrvani)?VMesicich(2)?_II_3"))
                .setEstimatedDurationInDays(
                        VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.(Nebo)?Dnech(2)?_II_3"))
                .setMaxBidsCount(VestnikTenderParserUtils.getFieldValue(form,
                        "FormItems\\.MaximalniPocet(Ucastniku)?_IV_1(_2)?"))
                .setEstimatedPrice(parseEstimatedPrice(form));

        // FURTHER INFORMATION PROVIDER, DOCUMENTATION PROVIDER, BIDS RECIPIENT, ON BEHALF OF
        final Element annexA = form.select("div.sub-content:has(div.iform-name:contains(Příloha A))").first();
        if (annexA != null) {
            tender.setFurtherInformationProvider(parseBodyFromAnnexSection(annexA, "I\\)"))
                    .setSpecificationsProvider(parseBodyFromAnnexSection(annexA, "II\\)"))
                    .setBidsRecipient(parseBodyFromAnnexSection(annexA, "III\\)"))
                    .addOnBehalfOf(parseBodyFromAnnexSection(annexA, "IV\\)"));
        }

        tender.setLots(parseLots(form));

        return tender;
    }

    /**
     * Parses bid deadline.
     *
     * @param root
     *         root element for parsing
     *
     * @return bid deadline
     */
    static String parseBidDeadline(final Element root) {
        final String deadlineDate = VestnikTenderParserUtils.getFieldValue(root,
                "FormItems\\.((Datum_IV_((3_4)|(2_3)|(2_2)))|(LhutaProDOruceni(Nabidek|Navrhu)" + "ZadostioUcast_IV_"
                        + "(" + "(3_4)|(4_3))))");
        final String deadlineTime = VestnikTenderParserUtils.getFieldValue(root,
                "FormItems\\.((LhutaProDOruceniNabidekZadostioUcastCas_IV_3_4)|(Cas_IV_((3_4)|(2_3)|(2_2)|(4_3))))");
        return VestnikTenderParserUtils.concatenateStrings(deadlineDate, deadlineTime);
    }

    /**
     * Parses tender estimated price.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return tender estimated price
     */
    private static ParsedPrice parseEstimatedPrice(final Document form) {
        // framework agreement estimated price is in a separate section
        if (BooleanUtils.toBoolean(VestnikTenderParser.isFrameworkAgreement(form))) {
            String netAmount = VestnikTenderParserUtils.getFieldValue(form,
                    "FormItems\\.((OdhadovanaHodnotaBezDph)|(UvedteOdhadovanouHodnotuBezDph))_II_(I|1)_4");
            // whole amount
            if (StringUtils.isNotEmpty(netAmount)) {
                return new ParsedPrice().setNetAmount(netAmount)
                        .setCurrency(VestnikTenderParserUtils.getSelectedOptionValue(form,
                                "FormItems\\.(MenaOdhadovanaHodnota|Mena1)_II_(I|1)_4"));
            } else {
                // range
                return new ParsedPrice().setMinNetAmount(
                        VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.(RozsahOd|Mezi)_II_(I|1)_4"))
                        .setMaxNetAmount(
                                VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.(RozsahDo|A)_II_(I|1)_4"))
                        .setCurrency(VestnikTenderParserUtils.getSelectedOptionValue(form,
                                "FormItems\\.(MenaRozsah|Mena2)_II_(I|1)_4"));
            }
        } else {
            return new ParsedPrice().setNetAmount(VestnikTenderParserUtils.getFieldValue(form,
                    "FormItems\\.((UvedtePredpokladanouHodnotuBezDph_II_((2_1)|5))|" + "" + ""
                            + "(OdhadovanaHodnotaBezDph_II_2_1))"))

                    .setMinNetAmount(VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.RozsahOd_II_((2_1)|5)"))
                    .setMaxNetAmount(VestnikTenderParserUtils.getFieldValue(form, "FormItems\\.RozsahDo_II_((2_1)|5)"))
                    .setCurrency(VestnikTenderParserUtils.getSelectedOptionValue(form,
                            "FormItems\\.Mena(Hodnota|Rozsah)_II_((2_1)|5)"));
        }
    }

    /**
     * Parses body information from specified sections from Annex. Together with section, body type must be specified.
     *
     * @param annex
     *         HTML fragment for form annex (eg. Annex A)
     * @param sectionHeadingRegex
     *         regular expression for selecting annex section by matching its heading
     *
     * @return parsed body information with all the available details
     */
    private static ParsedBody parseBodyFromAnnexSection(final Element annex, final String sectionHeadingRegex) {
        final Element section = annex.select(
                String.format("div.iform-subsection:matches(%s) + div + fieldset", sectionHeadingRegex)).first();
        if (section == null) {
            logger.warn("Parsing body from annex failed. Annex section for specified regex could not be found: {}",
                    sectionHeadingRegex);
            return null;
        }
        return VestnikTenderParserUtils.parseBody(section);
    }

    /**
     * Parses all the lots from contract notice.
     *
     * @param form
     *         parsed document for the source HTML page
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseLots(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();
        Elements lotsSections = getLotsSections(form);
        for (int i = 0; i < lotsSections.size(); i++) {
            lots.add(parseLot(lotsSections.get(i), i + 1));
        }
        return lots;
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
    private static Elements getLotsSections(final Document form) {
        Elements lotsForms = new Elements();

        // sometimes the <form> element is omitted and there are <fieldset>
        // elements as siblings of <div> element
        Elements fieldsets = form.select("div.iform-name:matches(Příloha B) ~ fieldset");
        for (int i = 0; i < fieldsets.size(); i += 3) {
            // by using the clone() function the original DOM tree is not modified, otherwise appending the elements
            // to a new element would remove them from original tree
            Element lotForm = form.createElement("form")
                    .appendChild(fieldsets.get(i).clone())
                    .appendChild(fieldsets.get(i + 1).clone())
                    .appendChild(fieldsets.get(i + 2).clone());
            lotsForms.add(lotForm);
        }

        // usually parts are in <form> element
        lotsForms.addAll(form.select("div.iform-name:matches(Příloha B) + div.clear + form"));

        return lotsForms;
    }

    /**
     * Parses tender lot from given lot form (HTML fragment).
     *
     * @param lotForm
     *         parsed tender lot form
     * @param lotPosition
     *         order in which the lot appears on the source page
     *
     * @return parsed tender lot
     */
    private static ParsedTenderLot parseLot(final Element lotForm, final int lotPosition) {
        return new ParsedTenderLot().setPositionOnPage(Integer.toString(lotPosition))
                .setLotNumber(VestnikTenderParserUtils.getFieldValue(lotForm, "AttItems\\.CastZakazkyC_0_0"))
                .setTitle(VestnikTenderParserUtils.getFieldValue(lotForm, "AttItems\\.Nazev_0_0"))
                .setDescription(VestnikTenderParserUtils.getTextAfterLabel(lotForm, "AttItems_StrucnyPopis_1"))
                .setEstimatedPrice(new ParsedPrice().setNetAmount(
                        VestnikTenderParserUtils.getFieldValue(lotForm, "AttItems\\.OdhadovanaHodnotaBezDph_3"))
                        .setMinNetAmount(VestnikTenderParserUtils.getFieldValue(lotForm, "AttItems\\.RozsahOd_3"))
                        .setMaxNetAmount(VestnikTenderParserUtils.getFieldValue(lotForm, "AttItems\\.RozsahDo_3"))
                        .setCurrency(VestnikTenderParserUtils.getSelectedOptionValue(lotForm, "AttItems\\.Mena.*")))
                .setEstimatedDurationInDays(VestnikTenderParserUtils.getFieldValue(lotForm, "AttItems\\.Dnech_4"))
                .setEstimatedDurationInMonths(
                        VestnikTenderParserUtils.getFieldValue(lotForm, "AttItems\\.DobaTrvaniVMesicich_4"))
                .setEstimatedStartDate(VestnikTenderParserUtils.getFieldValue(lotForm,
                        "AttItems\\.PredpokladaneDatumZahajeniStavebnichPraci_4"))
                .setEstimatedCompletionDate(VestnikTenderParserUtils.getFieldValue(lotForm,
                        "AttItems\\.PredpokladaneDatumDokonceniStavebnichPraci_4"))
                .setCpvs(parseLotCpvCodes(lotForm));
    }

    /**
     * Parses CPV codes for contract notice lot.
     *
     * @param lotForm
     *         parsed tender lot form
     *
     * @return list of parsed CPV codes
     */
    private static List<ParsedCPV> parseLotCpvCodes(final Element lotForm) {
        final List<ParsedCPV> cpvs = new ArrayList<>();
        // main CPV
        cpvs.add(new ParsedCPV().setIsMain(Boolean.TRUE.toString())
                .setCode(VestnikTenderParserUtils.getFieldValue(lotForm, "AttItems\\.HlavniSlovnikHp_2")));

        // other CPVs
        cpvs.addAll(lotForm.select("div.iform-field > input[name~=AttItems\\.HlavniSlovnikDp[1-4]_2")
                .stream()
                .filter(cpvDetail -> !cpvDetail.attr("value").isEmpty())
                .map(cpvDetail -> new ParsedCPV().setIsMain(Boolean.FALSE.toString()).setCode(cpvDetail.attr("value")))
                .collect(Collectors.toList()));
        return cpvs;
    }
}
