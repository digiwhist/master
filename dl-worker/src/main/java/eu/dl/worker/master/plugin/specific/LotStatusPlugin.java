package eu.dl.worker.master.plugin.specific;

import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.master.BaseMasterTenderLot;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.BaseMatchedTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.utils.BasePlugin;

import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;

/**
 * This plugin masters lot status field.
 *
 * @param <T>
 *            matched items type
 * @param <V>
 *            item type to be mastered
 * @param <U>
 *         context items type
 */
public class LotStatusPlugin<T extends BaseMatchedTenderLot, 
                             V extends BaseMasterTenderLot, U>
        extends BasePlugin implements MasterPlugin<T, V, U> {

    @Override
    public final V master(final List<T> matchedLots, final V finalItem, final List<U> context) {
        if (matchedLots == null) {
            return null;
        }
        
        TreeMap<LocalDate, T> lotStorage = new TreeMap<LocalDate, T>(Collections.reverseOrder());
        ((MasterTenderLot) finalItem).setStatus(null);
        
        for (T matchedLot: matchedLots) {
            LocalDate pubDate = matchedLot.getPublicationDate();
            if (pubDate != null) {
                lotStorage.put(pubDate, matchedLot);
            } else {
                lotStorage.put(LocalDate.MIN, matchedLot);
            }
        }
        
        for (Entry<LocalDate, T> entry : lotStorage.entrySet()) {
            if (((MatchedTenderLot) entry.getValue()).getStatus() == TenderLotStatus.AWARDED) {
                ((MasterTenderLot) finalItem).setStatus(TenderLotStatus.AWARDED);
                return finalItem;
            }
            
            if (((MatchedTenderLot) entry.getValue()).getStatus() == TenderLotStatus.FINISHED) {
                ((MasterTenderLot) finalItem).setStatus(TenderLotStatus.FINISHED);
                return finalItem;
            }
            
            if (((MatchedTenderLot) entry.getValue()).getStatus() == TenderLotStatus.CANCELLED) {
                ((MasterTenderLot) finalItem).setStatus(TenderLotStatus.CANCELLED);
                return finalItem;
            }
        }
        
        for (Entry<LocalDate, T> entry : lotStorage.entrySet()) {
            if (((MatchedTenderLot) entry.getValue()).getStatus() == TenderLotStatus.PREPARED) {
                ((MasterTenderLot) finalItem).setStatus(TenderLotStatus.PREPARED);
                return finalItem;
            }
            
            if (((MatchedTenderLot) entry.getValue()).getStatus() == TenderLotStatus.ANNOUNCED) {
                ((MasterTenderLot) finalItem).setStatus(TenderLotStatus.ANNOUNCED);
                return finalItem;
            }
        }
        
        return finalItem;
    }
}
