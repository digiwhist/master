package eu.digiwhist.worker.cz.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Handler for parsing form 18 - Contract award notice for contracts in the field of defence and security.
 */
public final class VestnikForm18Handler extends VestnikContractAwardHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VestnikForm18Handler() {
        throw new AssertionError();
    }

    /**
     * Parses Form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    public static ParsedTender parseFormAttributes(final ParsedTender tender, final Document form) {
        ParsedTender updatedTender = parseCommonContractAwardAttributes(tender, form);
        // set buyer type
        updatedTender.getBuyers().get(0).setBuyerType(parseBuyerType(form));
        // set buyer main activities
        updatedTender.getBuyers().get(0).setMainActivities(parseMainActivities(form));

        // set tender lots
        updatedTender.setLots(parseLots(form));

        return updatedTender;
    }

    /**
     * Returns the type of buyer (eg. ministry, national agency etc.).
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return type of the buyer
     */
    private static String parseBuyerType(final Document form) {
        final Element subsectionI2 = VestnikTenderParserUtils.getFormSubsectionByName(form, "^(I\\.)?2\\).*");
        String buyerType = VestnikTenderParserUtils.getCheckedInputValue(subsectionI2,
                "FormItems\\.Dvz_DruhVerejnehoZadavatele_I_2");

        // if "OTHER" is checked, parse also the specification of "OTHER"
        if (buyerType != null && buyerType.equalsIgnoreCase("OTHER")) {
            return VestnikTenderParserUtils.getFieldValue(subsectionI2, "FormItems\\.Dvz_Upresneni_I_2");
        } else {
            return buyerType;
        }
    }

    /**
     * Returns the list of provided main activities of the buyer.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return list of buyer main activities
     */
    private static List<String> parseMainActivities(final Document form) {
        List<String> activities = new ArrayList<>();
        final Element subsectionI3 = VestnikTenderParserUtils.getFormSubsectionByName(form, "^(I\\.)?3\\).*");

        // get values for all the checked options in subsection I.3)
        final Elements checkedOptions = subsectionI3.select("div.iform-field > input[checked]");
        for (Element activity : checkedOptions) {
            activities.add(VestnikTenderParserUtils.getLabelForField(activity, subsectionI3));
        }

        // if "Jiny" (other) is checked, parse also the specification of "Jiny" (for contracting authority)
        if (VestnikTenderParserUtils.isInputFieldChecked(subsectionI3, "FormItems\\.HpcJiny_I_3")) {
            activities.add(VestnikTenderParserUtils.getFieldValue(subsectionI3, "FormItems\\.HpcUpresneni1_I_3"));
        }

        // if "Jiny" (other) is checked, parse also the specification of "Jiny" (for contrating entity)
        if (VestnikTenderParserUtils.isInputFieldChecked(subsectionI3, "FormItems\\.HpcJinde_I_3")) {
            activities.add(VestnikTenderParserUtils.getFieldValue(subsectionI3, "FormItems\\.HpcUpresneni2_I_3"));
        }
        return activities;
    }
}

