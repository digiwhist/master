package eu.datlab.worker.ug.master;

import eu.datlab.worker.master.plugin.TenderBidPlugin;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.worker.master.plugin.AddressPlugin;
import eu.dl.worker.master.plugin.BaseMatchAndMasterPlugin;
import eu.dl.worker.master.plugin.CpvPlugin;
import eu.dl.worker.master.plugin.GeneralPricePlugin;
import eu.dl.worker.master.plugin.generic.LastPublishedPlugin;
import eu.dl.worker.master.plugin.generic.LastValuePlugin;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;
import eu.dl.worker.master.plugin.generic.ModusPlugin;
import eu.dl.worker.master.plugin.generic.UnionPlugin;
import eu.dl.worker.master.plugin.generic.comparators.StringComparator;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import eu.dl.worker.master.plugin.specific.AwardCriteriaPlugin;
import eu.dl.worker.master.plugin.specific.FundingsPlugin;
import eu.dl.worker.master.plugin.specific.LotStatusPlugin;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Plugin for mastering UG tender lots.
 */
public class GPPLotPlugin extends BaseMatchAndMasterPlugin<MatchedTender, MasterTender, MatchedTenderLot, MasterTenderLot> {

    @Override
    protected final List<List<MatchedTenderLot>> getListsForMatching(final List<MatchedTender> items) {
        if(items == null) {
            return null;
        }
        return items.stream().map(MatchedTender::getLotsWithStructuredId).filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    @Override
    protected final void registerNestedMasterPlugins() {
        nestedPluginRegistry
                .registerPlugin("MOD+LNN", new ModusPlugin<>(Arrays.asList(
                        "contractNumber", "estimatedDurationInMonths", "estimatedDurationInDays",
                        "estimatedDurationInYears", "maxFrameworkAgreementParticipants",
                        "envisagedCandidatesCount", "envisagedMinCandidatesCount",
                        "envisagedMaxCandidatesCount", "bidsCount", "validBidsCount", "electronicBidsCount",
                        "foreignCompaniesBidsCount", "lotNumber", "smeBidsCount", "otherEuMemberStatesCompaniesBidsCount",
                        "nonEuMemberStatesCompaniesBidsCount"), new TenderConverter()))
                .registerPlugin("LNN", new LastPublishedPlugin<>(Arrays.asList("awardDecisionDate",
                        "contractSignatureDate", "completionDate", "estimatedStartDate", "estimatedCompletionDate",
                        "cancellationDate", "cancellationReason",
                        "selectionMethod", "limitedCandidatesCountCriteria", "status", "isAwarded"), new TenderConverter()))
                .registerPlugin("LOR", new LogicalORPlugin<>(Arrays.asList(
                        "isElectronicAuction", "isFrameworkAgreement", "isDps", "isCoveredByGpa",
                        "areVariantsAccepted", "hasOptions")))
                .registerPlugin("Cpvs", new CpvPlugin<>())
                .registerPlugin("Price", new GeneralPricePlugin<>(Arrays.asList("estimatedPrice")))
                .registerPlugin("AwardCriteria", new AwardCriteriaPlugin<>())
                .registerPlugin("Bids", new TenderBidPlugin())
                .registerPlugin("Funding", new FundingsPlugin<>())
                .registerPlugin("Address", new AddressPlugin<>(Arrays.asList("addressOfImplementation")))
                .registerPlugin("Union", new UnionPlugin<>(Arrays.asList("amendments"), new TenderConverter()))
                .registerPlugin("Status", new LotStatusPlugin<>());

        // register Longest plugin in loop for each mastered value
        for (String s : new String[]{"title", "titleEnglish", "description", "descriptionEnglish",
                "eligibilityCriteria"}) {
            nestedPluginRegistry.registerPlugin("Longest" + s, new LastValuePlugin<>(
                    s, new StringComparator<>(s), new TenderConverter()));
        }
    }

    @Override
    protected final MasterTender setFinalList(final MasterTender finalItem, final List<MasterTenderLot> finalList) {
        if(finalItem == null) {
            return null;
        }
        return finalItem.setLots(finalList);
    }

    @Override
    protected final MasterTenderLot setSourceStructuredIds(final List<MatchedTenderLot> matchedList,
                                                     final MasterTenderLot masterItem) {
        if(masterItem == null || matchedList == null) {
            return null;
        }
        return masterItem
                .setSourceLotIds(matchedList.stream().filter(Objects::nonNull).map(MatchedTenderLot::getStructuredId)
                        .collect(Collectors.toList()));
    }

    @Override
    protected final MasterTenderLot createEmptyListItemInstance() {
        return new MasterTenderLot();
    }


    @Override
    protected final List<List<MatchedTenderLot>> match(final List<List<MatchedTenderLot>> inputListsForMatching) {
        if(inputListsForMatching == null) {
            return null;
        }
        List<MatchedTenderLot> tenderLots = new ArrayList<>();
        List<MatchedTenderLot> awardLots = new ArrayList<>();

        for (List<MatchedTenderLot> list : inputListsForMatching) {
            for (MatchedTenderLot lot : list) {
                if (lot.getEstimatedPrice() != null) {
                    tenderLots.add(lot);
                } else {
                    MatchedBid winningBid = getWinningBid(lot);
                    if (winningBid.getPrice() != null) {
                        awardLots.add(lot);
                    }
                }
            }
        }
        tenderLots.sort(Comparator.comparing(l -> l.getEstimatedPrice().getNetAmount()));
        awardLots.sort(Comparator.comparing(l -> getWinningBid(l).getPrice().getNetAmount()));
        int minSize = Math.min(tenderLots.size(), awardLots.size());
        List<List<MatchedTenderLot>> result = new ArrayList<>();
        for(int i = 0; i < minSize; i++) {
            List<MatchedTenderLot> oneLotGroup = new ArrayList<>();
            oneLotGroup.add(tenderLots.get(tenderLots.size() - 1 - i));
            oneLotGroup.add(awardLots.get(awardLots.size() - 1 - i));
            result.add(oneLotGroup);
        }

        if(tenderLots.size() > minSize) {
            for(int i = minSize; i < tenderLots.size(); i++) {
                List<MatchedTenderLot> oneLotGroup = new ArrayList<>();
                oneLotGroup.add(tenderLots.get(tenderLots.size() - 1 - i));
                result.add(oneLotGroup);
            }
        } else if(awardLots.size() > minSize) {
            for(int i = minSize; i < awardLots.size(); i++) {
                List<MatchedTenderLot> oneLotGroup = new ArrayList<>();
                oneLotGroup.add(awardLots.get(awardLots.size() - 1 - i));
                result.add(oneLotGroup);
            }
        }

        return result;
    }

    /**
     * Returns winning bid for given lot.
     * @param lot lot to get bid for
     * @return winning bid if present or empty bid otherwise
     */
    private static MatchedBid getWinningBid(final MatchedTenderLot lot) {
        if (lot == null || lot.getBids() == null) {
            return new MatchedBid();
        }
        return lot.getBids().stream().filter(Objects::nonNull).filter(MatchedBid::getIsWinning)
                .findFirst().orElse(new MatchedBid());
    }

}
