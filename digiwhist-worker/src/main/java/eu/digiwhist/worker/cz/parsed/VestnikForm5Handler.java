package eu.digiwhist.worker.cz.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Handler for parsing form 5 - Contract notice - utilities.
 */
public final class VestnikForm5Handler extends VestnikContractNoticeHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VestnikForm5Handler() {
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
        ParsedTender updatedTender = parseCommonContractNoticeAttributes(tender, form);
        // set buyer main activities
        updatedTender.getBuyers().get(0).setMainActivities(parseMainActivities(form));

        return updatedTender;
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
        final Element subsectionI2 = VestnikTenderParserUtils.getFormSubsectionByName(form, "^(I\\.)?2\\).*");

        // get values for all the checked options in subsection I.2)
        final Elements checkedOptions = subsectionI2.select("div.iform-field > input[checked]");
        for (Element activity : checkedOptions) {
            activities.add(VestnikTenderParserUtils.getLabelForField(activity, subsectionI2));
        }

        // if "Jiny" (other) is checked, parse also the specification of "Jiny"
        if (VestnikTenderParserUtils.isInputFieldChecked(subsectionI2, "FormItems\\.HpcJinde_I_2")) {
            activities.add(VestnikTenderParserUtils.getFieldValue(subsectionI2, "FormItems\\.HpcUpresneni_I_2"));
        }
        return activities;
    }
}
