package eu.digiwhist.worker.cz.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Handler for parsing form 51 - Contract/Design contest cancellation.
 */
public final class VestnikForm51Handler extends VestnikContractCancellationHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VestnikForm51Handler() {
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
        ParsedTender updatedTender = parseCommonContractCancellationAttributes(tender, form);
        // set buyer type
        updatedTender.getBuyers().get(0).setBuyerType(parseBuyerType(form));
        // set buyer main activities
        updatedTender.getBuyers().get(0).setMainActivities(parseMainActivities(form));

        return updatedTender;
    }

    /**
     * Returns the type of buyer (eg. ministry, national agency etc.)
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return type of the buyer
     */
    private static String parseBuyerType(final Document form) {
        final Element section2 = VestnikTenderParserUtils.getFormSubsectionByName(form, "^2\\).*");
        String buyerType = VestnikTenderParserUtils.getCheckedInputValue(section2,
                "FormItems\\.DruhVerejnehoZadavatele_I_2");

        // if "OTHER" is checked, parse also the specification of "OTHER"
        if (buyerType != null && buyerType.equalsIgnoreCase("OTHER")) {
            return VestnikTenderParserUtils.getFieldValue(section2, "FormItems\\.Dvz_ProsimSpecifikujte_I_2");
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
        final Element section2 = VestnikTenderParserUtils.getFormSubsectionByName(form, "^2\\).*");
        final Element mainActivitySection = section2.select("fieldset.form").get(1);

        // get values for all the checked options in section 2)
        Elements checkedOptions = mainActivitySection.select("div.iform-field > input[checked]");
        for (Element activity : checkedOptions) {
            activities.add(VestnikTenderParserUtils.getLabelForField(activity, mainActivitySection));
        }

        // if "Jiny" (other) is checked, parse also the specification of "Jiny" (contracting authority)
        if (VestnikTenderParserUtils.isInputFieldChecked(mainActivitySection, "FormItems\\.Jiny_I_2")) {
            activities.add(VestnikTenderParserUtils.getFieldValue(mainActivitySection,
                    "FormItems\\.Hpc_ProsimSpecifikujte_I_2"));
        }

        // main activities can be also in section 3)
        final Element section3 = VestnikTenderParserUtils.getFormSubsectionByName(form, "^3\\).*");
        checkedOptions = section3.select("div.iform-field > input[checked]");
        for (Element activity : checkedOptions) {
            activities.add(VestnikTenderParserUtils.getLabelForField(activity, section3));
        }

        return activities;
    }
}
