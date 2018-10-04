package eu.datlab.worker.master.indicator;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.utils.PopulateUtils;
import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.indicator.plugin.AdvertisementPeriodIndicatorPlugin;
import eu.dl.worker.indicator.plugin.CallForTenderIndicatorPlugin;
import eu.dl.worker.indicator.plugin.CentralizedProcurementIndicatorPlugin;
import eu.dl.worker.indicator.plugin.CoveredByGPAIndicatorPlugin;
import eu.dl.worker.indicator.plugin.DecisionPeriodIndicatorPlugin;
import eu.dl.worker.indicator.plugin.ElectronicAuctionIndicatorPlugin;
import eu.dl.worker.indicator.plugin.EnglishLanguageIndicatorPlugin;
import eu.dl.worker.indicator.plugin.FrameworkAgreementIndicatorPlugin;
import eu.dl.worker.indicator.plugin.IndicatorPlugin;
import eu.dl.worker.indicator.plugin.KeyMissingFieldsIndicatorPlugin;
import eu.dl.worker.indicator.plugin.NewCompanyIndicatorPlugin;
import eu.dl.worker.indicator.plugin.NoticeAndAwardDiscrepanciesIndicatorPlugin;
import eu.dl.worker.indicator.plugin.PoliticalConnectionsOfSuppliers;
import eu.dl.worker.indicator.plugin.ProcedureTypeIndicatorPlugin;
import eu.dl.worker.indicator.plugin.SingleBidIndicatorPlugin;
import eu.dl.worker.indicator.plugin.TaxHavenIndicatorPlugin;
import eu.dl.worker.utils.BasicPluginRegistry;
import eu.dl.worker.utils.PluginRegistry;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;

/**
 * This worker calculates indicators for tenders.
 *
 * @author Jakub Krafka
 */
public class IndicatorWorker extends BaseWorker {

    private static final String INCOMING_EXCHANGE_NAME = "master";

    private static final String OUTGOING_EXCHANGE_NAME = "master";

    private static final String VERSION = "1.0";

    private static TransactionUtils transactionUtils;

    private static MasterTenderDAO masterDao;

    private static MasterBodyDAO masterBodyDao;

    private static PopulateUtils populateUtils;

    protected PluginRegistry<IndicatorPlugin<MasterTender>> indicatorPluginRegistry = new BasicPluginRegistry();

    /**
     * Initialization of everythong.
     */
    public IndicatorWorker() {
        super();
        transactionUtils = DAOFactory.getDAOFactory().getTransactionUtils();

        masterDao = DAOFactory.getDAOFactory().getMasterTenderDAO(getName(), VERSION);

        masterBodyDao = DAOFactory.getDAOFactory().getMasterBodyDAO(getName(), VERSION);

        populateUtils = new PopulateUtils(masterBodyDao);

        config.addConfigFile("indicator");

        registerIndicatorPlugins();
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected final String getOutgoingExchangeName() {
        return OUTGOING_EXCHANGE_NAME;
    }

    @Override
    public final void doWork(final Message message) {
        transactionUtils.begin();

        String id = message.getValue("id");

        final MasterTender tender = masterDao.getById(id);

        if (tender != null) {
            populateUtils.populateBodies(Arrays.asList(tender));

            List<Indicator> indicators = new ArrayList<>();

            // iterate over all indicator plugins and execute them in a proper order
            for (Entry<String, IndicatorPlugin<MasterTender>> entry : indicatorPluginRegistry.getPlugins().entrySet()) {
                IndicatorPlugin<MasterTender> plugin = entry.getValue();
                Indicator indicator = (Indicator) plugin.evaluate(tender);
                if (indicator != null) {
                    indicators.add(indicator);
                }
            }

            tender.setIndicators(indicators);

            populateUtils.depopulateBodies(Arrays.asList(tender));
            masterDao.save(tender);
        }

        transactionUtils.commit();
    }

    @Override
    protected final void resend(final String version, final String dateFrom, final String dateTo) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    /**
     * Registers indicator plugins.
     */
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

        NewCompanyIndicatorPlugin newCompanyIndicatorPlugin =
                new NewCompanyIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                newCompanyIndicatorPlugin.getType(), newCompanyIndicatorPlugin);

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

        TaxHavenIndicatorPlugin taxHavenPlugin =
                new TaxHavenIndicatorPlugin();
        indicatorPluginRegistry.registerPlugin(
                taxHavenPlugin.getType(), taxHavenPlugin);

        KeyMissingFieldsIndicatorPlugin keyMissingFieldsPlugin = new KeyMissingFieldsIndicatorPlugin(
                DAOFactory.getDAOFactory().getMatchedTenderDAO(null, null, Collections.emptyList()),
                DAOFactory.getDAOFactory().getCleanTenderDAO(null, null));
        indicatorPluginRegistry.registerPlugin(keyMissingFieldsPlugin.getType(), keyMissingFieldsPlugin);

        NoticeAndAwardDiscrepanciesIndicatorPlugin noticeAndAwardDiscPlugin =
                new NoticeAndAwardDiscrepanciesIndicatorPlugin(
                        DAOFactory.getDAOFactory().getMatchedTenderDAO(null, null, Collections.emptyList()));
        indicatorPluginRegistry.registerPlugin(noticeAndAwardDiscPlugin.getType(), noticeAndAwardDiscPlugin);

        PoliticalConnectionsOfSuppliers politicalConnectionsOfSuppliers = new PoliticalConnectionsOfSuppliers(
                masterBodyDao);
        indicatorPluginRegistry.registerPlugin(politicalConnectionsOfSuppliers.getType(),
                politicalConnectionsOfSuppliers);
    }
}
