package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Abstract class for tender indicators calculated as an average of results of lot plugin applied on each lot.
 */
public abstract class LotAverageIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    private final LotIndicatorPlugin lotPlugin;

    /**
     * Default constructor sets the lot level indicator plugin.
     *
     * @param lotPlugin
     *      lot level indicator plugin
     */
    public LotAverageIndicatorPlugin(final LotIndicatorPlugin lotPlugin) {
        super();
        this.lotPlugin = lotPlugin;
    }

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null || tender.getLots() == null || tender.getLots().isEmpty()) {
            return insufficient();
        }

        // init metadata
        HashMap<String, Object> metaData = new HashMap<>();
        metaData.put("lotsMetaData", new ArrayList<Map<String, Object>>());
        List<Map<String, Object>> lotsMetaData = (List<Map<String, Object>>) metaData.get("lotsMetaData");

        double avg = 0;
        int avgMembersCount = 0;
        Indicator nonCalculatedIndicator = null;
        for (MasterTenderLot lot : tender.getLots()) {
            Indicator result = lotPlugin.evaluate(lot, tender);

            // metaData for particular lot
            Map<String, Object> metaDataItem = new HashMap<>();
            metaDataItem.put("lotTitle", lot.getTitle());
            metaDataItem.put("indicatorStatus", result.getStatus());
            metaDataItem.put("indicatorValue", result.getValue());
            metaDataItem.put("indicatorMetaData", result.getMetaData());
            lotsMetaData.add(metaDataItem);

            // average value for calculated indicators
            if (result.getStatus() == IndicatorStatus.CALCULATED) {
                avgMembersCount++;
                avg = avg + ((result.getValue() - avg) / avgMembersCount);
            } else {
                nonCalculatedIndicator = result;
            }
        }

        // membersCount = 0 means no calculated indicators
        if (avgMembersCount == 0) {
            if (tender.getLots().size() == 1) {
                // for single lot return original lot indicator result with updated metadata and type
                nonCalculatedIndicator.setMetaData(metaData);
                nonCalculatedIndicator.setType(getType());
                return nonCalculatedIndicator;
            } else {
                // for multi-lot return undefined
                undefined();
            }
        }

        // if any lot plugin is calculated return average calculated value
        return calculated(avg, metaData);
    }
}
