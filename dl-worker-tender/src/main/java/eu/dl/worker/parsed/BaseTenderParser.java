package eu.dl.worker.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.Raw;

import java.util.ArrayList;
import java.util.List;

/**
 * Base class for tender parsers. Waits for raw tender data, parses it and saves
 * to database.
 * 
 * @param <T>
 *            parsable item type
 * 
 * @param <V>
 *            raw item
 */
public abstract class BaseTenderParser<V extends Raw, T extends ParsedTender> extends BaseParser<V, T> {

    @Override
    protected final List<T> postProcess(final List<T> parsedTenders, final V raw) {
        List<T> processed = new ArrayList<T>();
        for (T parsedTender : parsedTenders) {
            // set country of origin
            String country = countryOfOrigin(parsedTender, raw);
            if (country != null) {
                parsedTender.setCountry(country);
            }
            processed.add(setLotsAndBidsIds(parsedTender));
        }
        return processed;
    }

    /**
     * Method sets id for each lot and their bids of the parsed tender.
     * 
     * @param parsedTender
     *      parsed tender
     * @return parsed tender with updated lots and bids
     */
    private T setLotsAndBidsIds(final T parsedTender) {
        if (parsedTender.getLots() != null) {
            int lotId = 1;
            for (ParsedTenderLot lot : parsedTender.getLots()) {
                lot.setLotId(String.valueOf(lotId++));
                if (lot.getBids() != null) {
                    int bidId = 1;
                    for (ParsedBid bid : lot.getBids()) {
                        if (bid != null) {
                            bid.setBidId(String.valueOf(bidId++));
                        }
                    }
                }
            }
        }

        return parsedTender;
    }

    /**
     * Returns country of origin for parsed item. Returns null, in case there should be no
     * value set (i.e. the value is set in parser itself).
     *
     * @param parsed
     *            Parsed items for which should be
     * @param raw
     *            raw item - "source" of parsed item
     *
     * @return country of origin - country ISO code or null, in case there should be no country set.
     */
    protected abstract String countryOfOrigin(T parsed, V raw);
}
