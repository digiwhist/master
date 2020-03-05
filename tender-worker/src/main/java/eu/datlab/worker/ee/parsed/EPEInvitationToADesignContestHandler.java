package eu.datlab.worker.ee.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


/**
 * 'Invitation to a design contest' handler for E-procurement in Estonia.
 *
 */
public final class EPEInvitationToADesignContestHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private EPEInvitationToADesignContestHandler() {
        throw new AssertionError();
    }

    /**
     * Parses 'invitation to a design contest' form.
     *
     * @param doc
     *      parsed document
     * @param publicationDate
     *      publication date
     * @return parsed tender
     */
    public static ParsedTender parse(final Document doc, final String publicationDate) {
        Element context = EPEParserUtils.getDataTable(doc);

        return EPEParserUtils.parsePublicationAndTitle(doc, publicationDate)
                .addBuyer(EPEParserUtils.parseBuyer(context))
                .setBidDeadline(EPEParserUtils.tableValueByLabel("^IV\\.4\\.3\\)", context))
                .setCpvs(EPEParserUtils.parseCPVs("^II\\.1\\.3\\)", context))
                .setEligibleBidLanguages(parseEligibleLanguages(context))
                .addFunding(EPEParserUtils.parseEUFunding("^VI\\.1\\)", context))
                .setNationalProcedureType(EPEParserUtils.tableValueByLabel("^IV\\.1\\)", context));
    }

    /**
     * @param context
     *         context that includes eligible languages
     * @return non-empty list of languages or null
     */
    private static List<String> parseEligibleLanguages(final Element context) {
        List<String> resList = new ArrayList<>();
        String languages = EPEParserUtils.tableValueByLabel("Keeled, milles võib esitada projekte või osalemistaotlusi",
                context);
        if (languages == null) {
            return null;
        }
        while (languages != null) {
            resList.addAll(Arrays.asList(languages.split(", ?")));
            String nextTitle = EPEParserUtils.nextNNodeValueFromNextTableRowByValue(languages, 0, context);
            languages = EPEParserUtils.nextNNodeValueFromNextTableRowByValue(languages, 1, context);
            if (nextTitle == null || !nextTitle.isEmpty()) {
                break;
            }
        }
        return resList;
    }

}


