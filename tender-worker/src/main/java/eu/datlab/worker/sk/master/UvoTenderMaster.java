package eu.datlab.worker.sk.master;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import eu.datlab.worker.master.BaseDatlabTenderMaster;
import eu.datlab.worker.master.MasterUtils;
import eu.datlab.worker.master.plugin.TenderLotDPSPlugin;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.utils.DTOUtils;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;
import org.apache.commons.lang3.ObjectUtils;

/**
 * Master for UVO tenders.
 */
public class UvoTenderMaster extends BaseDatlabTenderMaster {
    /**
     * Worker version.
     */
    public static final String VERSION = "1.0";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry.unRegisterPlugin("Lots");

        pluginRegistry.registerPlugin("LotsDPS", new TenderLotDPSPlugin());
    }

    @Override
    protected final List<MatchedTender> sourceSpecificPreprocessData(final List<MatchedTender> items) {
        MasterTender tender = new MasterTender();

        LogicalORPlugin plugin = new LogicalORPlugin(Arrays.asList("isFrameworkAgreement"));
        tender = (MasterTender) plugin.master(items, tender, items);

        if (Boolean.TRUE.equals(tender.getIsFrameworkAgreement())) {
            LocalDate minAwardDate = items.stream()
                .filter(MasterUtils::isContractAward)
                .map(DTOUtils::getPublicationDate)
                .min(LocalDate::compareTo).orElse(null);

            items.stream()
                .filter(MasterUtils::isContractAward)
                .filter(t -> {
                    LocalDate publicationDate = DTOUtils.getPublicationDate(t);
                    return ObjectUtils.allNotNull(publicationDate, minAwardDate) && publicationDate.isAfter(minAwardDate);
                })
                .forEach(t -> {
                    t.getPublications().stream()
                        .filter(p -> Boolean.TRUE.equals(p.getIsIncluded()) && p.getFormType() == PublicationFormType.CONTRACT_AWARD)
                        .forEach(p -> p.setFormType(PublicationFormType.CONTRACT_IMPLEMENTATION));
                });
        }

        List preprocessedData = items.stream()
            .filter(isNotContractImplementation())
            .filter(isNotCorrectionOfSummaryReport())
            .collect(Collectors.toList());

        return preprocessedData;
    }
    
    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        MasterUtils.reduceLotsWithoutBids(item);
        return item;
    }

    /**
     * Predicate used to filter the resulting set of items. It does skip tenders
     * with publication type = CONTRACT_UPDATE if the sourceFormType indicates that it is a correction of
     * a summary report which contains multiple tenders. We don't have a mechanism for application such correction.
     *
     * @return predicate used to test type and isIncluded of tender.
     */
    private static Predicate<MatchedTender> isNotCorrectionOfSummaryReport() {
        return new Predicate<MatchedTender>() {

            @Override
            public boolean test(final MatchedTender t) {
                for (Publication publication : t.getPublications()) {
                    if (publication.getIsIncluded() != null
                            && publication.getIsIncluded()
                            && publication.getFormType() != null
                            && publication.getFormType().equals(PublicationFormType.CONTRACT_UPDATE)
                            && (publication.getSourceFormType().startsWith("IN")
                            || publication.getSourceFormType().startsWith("IE")
                            || publication.getSourceFormType().startsWith("IV"))) {

                        return false;
                    }
                }
                return true;
            }

        };
    }
}
