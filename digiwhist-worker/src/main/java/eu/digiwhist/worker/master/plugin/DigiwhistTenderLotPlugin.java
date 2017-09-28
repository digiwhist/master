package eu.digiwhist.worker.master.plugin;

import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.worker.master.plugin.generic.comparators.StringComparator;
import eu.dl.worker.master.plugin.AddressPlugin;
import eu.dl.worker.master.plugin.BaseTenderLotPlugin;
import eu.dl.worker.master.plugin.CpvPlugin;
import eu.dl.worker.master.plugin.GeneralPricePlugin;
import eu.dl.worker.master.plugin.generic.LastPublishedPlugin;
import eu.dl.worker.master.plugin.generic.LastValuePlugin;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;
import eu.dl.worker.master.plugin.generic.ModusPlugin;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import eu.dl.worker.master.plugin.specific.AwardCriteriaPlugin;
import eu.dl.worker.master.plugin.specific.FundingsPlugin;
import eu.dl.worker.master.plugin.specific.LotStatusPlugin;

import java.util.Arrays;

/**
 * Tender Lot Mastering plugin. Matches lots of matched tenders and creates master record for each lot.
 */
public final class DigiwhistTenderLotPlugin extends BaseTenderLotPlugin<MatchedTender, MasterTender, MatchedTenderLot,
        MasterTenderLot> {

    @Override
    protected MasterTenderLot createEmptyListItemInstance() {
        return new MasterTenderLot();
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void registerNestedMasterPlugins() {
        nestedPluginRegistry
                .registerPlugin("MOD+LNN", new ModusPlugin<>(Arrays.asList(
                        "contractNumber", "estimatedDurationInMonths", "estimatedDurationInDays",
                        "estimatedDurationInYears", "maxFrameworkAgreementParticipants",
                        "envisagedCandidatesCount", "envisagedMinCandidatesCount",
                        "envisagedMaxCandidatesCount", "bidsCount", "validBidsCount", "electronicBidsCount",
                        "foreignCompaniesBidsCount"),
                        new TenderConverter()))
                .registerPlugin("LNN", new LastPublishedPlugin<>(Arrays.asList("awardDecisionDate",
                        "contractSignatureDate", "completionDate", "cancellationDate", "cancellationReason",
                        "selectionMethod", "limitedCandidatesCountCriteria", "status"), new TenderConverter()))
                .registerPlugin("LOR", new LogicalORPlugin<>(Arrays.asList(
                        "isElectronicAuction", "isFrameworkAgreement", "isDps", "isCoveredByGpa",
                        "areVariantsAccepted", "hasOptions")))
                .registerPlugin("Cpvs", new CpvPlugin<>())
                .registerPlugin("Price", new GeneralPricePlugin<>(Arrays.asList("estimatedPrice")))
                .registerPlugin("AwardCriteria", new AwardCriteriaPlugin<>())
                .registerPlugin("Bids", new DigiwhistBidPlugin())
                .registerPlugin("Funding", new FundingsPlugin<>())
                .registerPlugin("Address", new AddressPlugin<>(Arrays.asList("addressOfImplementation")))
                .registerPlugin("Status", new LotStatusPlugin<>());

        // register Longest plugin in loop for each mastered value
        for (String s : new String[]{"title", "titleEnglish", "description", "descriptionEnglish",
                "eligibilityCriteria"}) {
            nestedPluginRegistry.registerPlugin("Longest" + s, new LastValuePlugin<>(
                    s, new StringComparator<>(s), new TenderConverter()));
        }
    }
}
