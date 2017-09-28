package eu.dl.worker.master.plugin.generic.converter;

import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.utils.InitUtils;

import java.util.List;

/**
 * This is converter used in tender mastering. Its able to convert matched
 * to master objects for tender specific parts.
 */
public class TenderConverter implements Converter {
    @Override
    public final Object convert(final Object object) {
        if (object == null) {
            return null;
        }

        if (object instanceof List) {
            List<?> objects = (List<?>) object;
            // check if we need to convert matched to master, optionally convert
            if (objects.get(0) != null && objects.get(0) instanceof MatchedBody) {
                return InitUtils.matchedToMasterBody((List<MatchedBody>) objects);
            } else if (objects.get(0) != null && objects.get(0) instanceof MatchedBid) {
                return InitUtils.matchedToMasterBid((List<MatchedBid>) objects);
            } else if (objects.get(0) != null && objects.get(0) instanceof MatchedTenderLot) {
                return InitUtils.matchedToMasterLot((List<MatchedTenderLot>) objects);
            }
        }

        if (object instanceof MatchedBody) {
            return InitUtils.matchedToMasterBody((MatchedBody) object);
        }

        return object;
    }
}
