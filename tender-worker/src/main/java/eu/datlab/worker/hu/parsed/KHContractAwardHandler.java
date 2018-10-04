package eu.datlab.worker.hu.parsed;

import org.jsoup.nodes.Document;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Handler for Contract Award Notice on KH.
 *
 * @author Marek Mikes
 */
final class KHContractAwardHandler {
    /**
     * Private constructor to make this class static.
     */
    private KHContractAwardHandler() {
    }

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
    public static ParsedTender parse(final ParsedTender tender, final Document form) {
        // todo implement

        return tender;
    }
}
