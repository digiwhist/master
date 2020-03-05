package eu.datlab.worker.master;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.worker.master.plugin.TenderDocumentPlugin;
import eu.datlab.worker.master.plugin.TenderLotPlugin;
import eu.dl.dataaccess.dao.MasterDAO;
import eu.dl.dataaccess.dao.MatchedDAO;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.indicator.plugin.AdvertisementPeriodIndicatorPlugin;
import eu.dl.worker.indicator.plugin.CallForTenderIndicatorPlugin;
import eu.dl.worker.indicator.plugin.CentralizedProcurementIndicatorPlugin;
import eu.dl.worker.indicator.plugin.CoveredByGPAIndicatorPlugin;
import eu.dl.worker.indicator.plugin.DecisionPeriodIndicatorPlugin;
import eu.dl.worker.indicator.plugin.ElectronicAuctionIndicatorPlugin;
import eu.dl.worker.indicator.plugin.EnglishLanguageIndicatorPlugin;
import eu.dl.worker.indicator.plugin.FrameworkAgreementIndicatorPlugin;
import eu.dl.worker.indicator.plugin.NoticeAndAwardDiscrepanciesIndicatorPlugin;
import eu.dl.worker.indicator.plugin.ProcedureTypeIndicatorPlugin;
import eu.dl.worker.indicator.plugin.SingleBidIndicatorPlugin;
import eu.dl.worker.master.BaseTenderMaster;
import eu.dl.worker.master.plugin.AddressPlugin;
import eu.dl.worker.master.plugin.CancellationFieldsPlugin;
import eu.dl.worker.master.plugin.CpvPlugin;
import eu.dl.worker.master.plugin.GeneralPricePlugin;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.master.plugin.body.BodiesPlugin;
import eu.dl.worker.master.plugin.generic.LastPublishedPlugin;
import eu.dl.worker.master.plugin.generic.LastValuePlugin;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;
import eu.dl.worker.master.plugin.generic.ModusPlugin;
import eu.dl.worker.master.plugin.generic.UnionPlugin;
import eu.dl.worker.master.plugin.generic.comparators.StringComparator;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import eu.dl.worker.master.plugin.specific.AwardCriteriaPlugin;
import eu.dl.worker.master.plugin.specific.FundingsPlugin;
import eu.dl.worker.master.plugin.specific.RobustPricePlugin;

import java.util.Arrays;

/**
 * Base class for body masters.
 */
public abstract class BaseDatlabTenderMaster extends BaseTenderMaster<MatchedTender, MasterTender> {
    @Override
    protected final MatchedDAO getMatchedDAO() {
        return DAOFactory.getDAOFactory().getMatchedTenderDAO(getName(), getVersion(), null);
    }

    @Override
    protected final MasterDAO getMasterDAO() {
        return DAOFactory.getDAOFactory().getMasterTenderDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerProjectSpecificPlugins() {
        MasterPlugin p = new ModusPlugin<>(Arrays.asList("buyerAssignedId", "title", "titleEnglish",
            "procedureType", "nationalProcedureType", "isAcceleratedProcedure",
            "acceleratedProcedureJustification", "description", "descriptionEnglish", "maxBidsCount",
            "supplyType", "size", "sizeNational", "furtherInformationProvider",
            "specificationsProvider", "bidsRecipient", "specificationsCreator", "appealBodyName",
            "mediationBodyName", "maxFrameworkAgreementParticipants", "estimatedDurationInMonths",
            "estimatedDurationInDays", "estimatedDurationInYears", "envisagedCandidatesCount",
            "envisagedMinCandidatesCount", "envisagedMaxCandidatesCount", "awardDeadlineDuration",
            "country"), new TenderConverter());
        pluginRegistry
                .registerPlugin("MOD+LNN", p)
                .registerPlugin("LNN", new LastPublishedPlugin<>(Arrays.asList("documentsDeadline",
                        "estimatedStartDate", "estimatedCompletionDate", "awardDecisionDate",
                        "contractSignatureDate", "limitedCandidatesCountCriteria", "selectionMethod",
                        "cancellationDate", "cancellationReason", "isWholeTenderCancelled", "enquiryDeadline",
                        "awardDeadline", "bidDeadline", "isAwarded"), new TenderConverter()))
                .registerPlugin("LOR", new LogicalORPlugin<>(Arrays.asList("documentsPayable",
                        "isDocumentsAccessRestricted", "isCentralProcurement", "isJointProcurement", "isOnBehalfOf",
                        "hasLots", "areVariantsAccepted", "hasOptions", "isCoveredByGpa", "isFrameworkAgreement",
                        "isDps", "isElectronicAuction", "isEInvoiceAccepted")))
                .registerPlugin("Bodies", new BodiesPlugin<>(Arrays.asList("buyers", "onBehalfOf", "administrators",
                        "supervisors", "candidates", "approachedBidders")))
                .registerPlugin("Union", new UnionPlugin<>(Arrays.asList("courtProceedings", "courtInterventions",
                        "npwpReasons", "eligibleBidLanguages", "corrections", "publications"), new TenderConverter()))
                .registerPlugin("Prices", new GeneralPricePlugin<>(Arrays.asList("documentsPrice", "estimatedPrice",
                        "finalPrice")))
                .registerPlugin("Lots", new TenderLotPlugin())
                .registerPlugin("Cpvs", new CpvPlugin<>())
                .registerPlugin("Fundings", new FundingsPlugin<>())
                .registerPlugin("AwardCriteria", new AwardCriteriaPlugin<>())
                .registerPlugin("Cancellation", new CancellationFieldsPlugin<>(Arrays.asList("cancellationDate",
                        "cancellationReason", "isWholeTenderCancelled")))
                .registerPlugin("Address", new AddressPlugin<>(Arrays.asList("documentsLocation",
                        "addressOfImplementation")))
                .registerPlugin("Documents", new TenderDocumentPlugin())
                .registerPlugin("RobustPrice", new RobustPricePlugin());

        // register Longest plugin in loop for each mastered value
        for (String s : new String[]{"deposits", "eligibilityCriteria", "personalRequirements", "economicRequirements",
                "technicalRequirements", "excessiveFrameworkAgreementJustification"}) {
            pluginRegistry.registerPlugin("Longest" + s, new LastValuePlugin<>(
                    s, new StringComparator<>(s), new TenderConverter()));
        }
    }

    @Override
    protected final void registerIndicatorPlugins() {
        SingleBidIndicatorPlugin singleBidIndicatorPlugin = new SingleBidIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(singleBidIndicatorPlugin.getType(), singleBidIndicatorPlugin);

        CentralizedProcurementIndicatorPlugin centralizedProcurementIndicatorPlugin =
                new CentralizedProcurementIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                centralizedProcurementIndicatorPlugin.getType(), centralizedProcurementIndicatorPlugin);

        AdvertisementPeriodIndicatorPlugin advertisementPeriodIndicatorPlugin =
                new AdvertisementPeriodIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                advertisementPeriodIndicatorPlugin.getType(), advertisementPeriodIndicatorPlugin);

        DecisionPeriodIndicatorPlugin decisionPeriodIndicatorPlugin =
                new DecisionPeriodIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                decisionPeriodIndicatorPlugin.getType(), decisionPeriodIndicatorPlugin);

        CoveredByGPAIndicatorPlugin coveredByGPAIndicatorPlugin =
                new CoveredByGPAIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                coveredByGPAIndicatorPlugin.getType(), coveredByGPAIndicatorPlugin);

        ElectronicAuctionIndicatorPlugin electronicAuctionIndicatorPlugin =
                new ElectronicAuctionIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                electronicAuctionIndicatorPlugin.getType(), electronicAuctionIndicatorPlugin);

        FrameworkAgreementIndicatorPlugin frameworkAgreementIndicatorPlugin =
                new FrameworkAgreementIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                frameworkAgreementIndicatorPlugin.getType(), frameworkAgreementIndicatorPlugin);

        EnglishLanguageIndicatorPlugin englishLanguagePlugin =
                new EnglishLanguageIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                englishLanguagePlugin.getType(), englishLanguagePlugin);

        CallForTenderIndicatorPlugin priorInformationNoticePlugin =
                new CallForTenderIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                priorInformationNoticePlugin.getType(), priorInformationNoticePlugin);
        
        ProcedureTypeIndicatorPlugin procedureTypePlugin =
                new ProcedureTypeIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                procedureTypePlugin.getType(), procedureTypePlugin);

        NoticeAndAwardDiscrepanciesIndicatorPlugin noticeAndAwardDiscPlugin =
            new NoticeAndAwardDiscrepanciesIndicatorPlugin((MatchedTenderDAO) getMatchedDAO());
        indicatorPluginRegistry.registerPlugin(noticeAndAwardDiscPlugin.getType(), noticeAndAwardDiscPlugin);
    }
}
