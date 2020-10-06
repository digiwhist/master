package eu.datlab.worker.matched;

import com.google.common.collect.Sets;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.DigestUtils;
import org.apache.lucene.search.spell.NGramDistance;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * This plugin matches tenders based on their similarity.
 */
public class TenderSimilarityMatchingPlugin extends BaseTenderMatchingPlugin {

    private static final String MATCHED_BY = "similarity";
    protected static final double SIMILARITY_THRESHOLD = 1.5;

    protected MatchedBodyDAO matchedBodyDao;
    private final String matcher;

    /**
     * Constructor.
     * @param dao tender dao
     * @param matcher matcher name
     * @param bodyDao body dao
     * @param isStrict enables/disables strict mode
     */
    public TenderSimilarityMatchingPlugin(final MatchedTenderDAO dao,
                                          final MatchedBodyDAO bodyDao, final String matcher, final boolean isStrict) {
        super(dao, isStrict);
        matchedBodyDao = bodyDao;
        this.matcher = matcher;
    }

    /**
     * Constructor.
     * @param dao tender dao
     * @param matcher matcher name
     * @param bodyDao body dao
     */
    public TenderSimilarityMatchingPlugin(final MatchedTenderDAO dao,
                                          final String matcher, final MatchedBodyDAO bodyDao) {
        super(dao);
        matchedBodyDao = bodyDao;
        this.matcher = matcher;
    }

    @Override
    public final String getMatchedBy() {
        return MATCHED_BY;
    }

    @Override
    public final List<MatchedTender> getMatchedTenders(final MatchedTender matchedTender) {
        List<MatchedBody> originalBuyers = matchedTender.getBuyers();
        matchedTender.setBuyers(getPopulatedBuyers(matchedTender));

        List<String> buyersGroupIds = null;
        List<String> buyersNames = null;

        if (matchedTender.getBuyers() != null) {
            buyersGroupIds = matchedTender.getBuyers().stream().filter(Objects::nonNull)
                    .map(MatchedBody::getGroupId).filter(Objects::nonNull).collect(Collectors.toList());
            buyersNames = matchedTender.getBuyers().stream().filter(Objects::nonNull)
                    .map(MatchedBody::getName).filter(Objects::nonNull).collect(Collectors.toList());
        }

        String title = matchedTender.getTitle();
        String buyerAssignedId = matchedTender.getBuyerAssignedId();
        String sourceTenderId = null;
        if (matchedTender.getPublications() != null) {
            sourceTenderId = matchedTender.getPublications().stream().filter(Objects::nonNull)
                    .filter(p -> p.getIsIncluded() != null).filter(Publication::getIsIncluded)
                    .map(Publication::getSourceTenderId).filter(Objects::nonNull).findFirst().orElse(null);
        }


        List<MatchedTender> matchedItemsPool = matchedTenderDao.getByBuyerAssignedId(buyerAssignedId);

        matchedItemsPool.removeIf(t -> !t.getCreatedBy().equals(matcher));
        logger.info("Pool of {} matched tenders selected as possible match by buyer assigned id " + buyerAssignedId,
                matchedItemsPool.size());
        System.out.println("Pool size: " + matchedItemsPool.size());
        for (MatchedTender possibleMatchTender : matchedItemsPool) {
            double similarityForPair = computeSimilarity(buyersGroupIds, buyersNames, title, sourceTenderId,
                    possibleMatchTender);
            if (similarityForPair < SIMILARITY_THRESHOLD) {
                matchedItemsPool.remove(possibleMatchTender);
            }
        }
        System.out.println("Pool size: " + matchedItemsPool.size());
        // set original buyers that contain only group ids
        matchedTender.setBuyers(originalBuyers);
        matchedItemsPool.forEach(this::depopulateBuyers);
        return matchedItemsPool;
    }

    /**
     * Gets matched buyers by their ids in matched tender.
     *
     * @param tender matched tender
     * @return list of matched buyers
     */
    private List<MatchedBody> getPopulatedBuyers(final MatchedTender tender) {
        if (tender.getBuyers() == null) {
            return null;
        }
        List<MatchedBody> result = new ArrayList<>();
        for (MatchedBody buyer : tender.getBuyers()) {
            String buyerId = buyer.getId();
            if (buyerId != null && !buyerId.isEmpty()) {
                MatchedBody populatedBuyer = matchedBodyDao.getById(buyerId);
                if (populatedBuyer != null) {
                    result.add(populatedBuyer);
                }
            }
        }
        return result;
    }

    /**
     * Depopulates buyers for matched tender - sets new buyers which contain only id, groupId and completenessScore.
     *
     * @param tender matched tender
     * @return matched tender with depopulated buyers
     */
    private MatchedTender depopulateBuyers(final MatchedTender tender) {
        if (tender.getBuyers() == null) {
            return tender;
        }
        List<MatchedBody> depopulatedBuyers = new ArrayList<>();
        for (MatchedBody buyer : tender.getBuyers()) {
            MatchedBody newBuyer = new MatchedBody();
            newBuyer.setId(buyer.getId());
            newBuyer.setGroupId(buyer.getGroupId());
            newBuyer.setCompletenessScore(buyer.getCompletenessScore());
            depopulatedBuyers.add(newBuyer);
        }
        tender.setBuyers(depopulatedBuyers);
        return tender;
    }


    /**
     * Computes similarity for two tenders, first is represented by values of fields, second is matched tender.
     *
     * @param buyersGroupIds1  buyersGroupIds for first tender
     * @param buyersNames1     buyersNames for first tender
     * @param title1           title for first tender
     * @param sourceTenderId1  sourceTenderId for first tender
     * @param t2               second tender
     * @return similarity score
     */
    public final double computeSimilarity(final List<String> buyersGroupIds1, final List<String> buyersNames1,
                                          final String title1,
                                          final String sourceTenderId1,
                                          final MatchedTender t2) {
        List<String> buyersGroupIds2 = null;
        List<String> buyersNames2 = null;
        if (t2.getBuyers() != null) {
            buyersGroupIds2 = t2.getBuyers().stream().filter(Objects::nonNull)
                    .map(MatchedBody::getGroupId).filter(Objects::nonNull).collect(Collectors.toList());
        }

        String title2 = t2.getTitle();
        String sourceTenderId2 = null;
        if (t2.getPublications() != null) {
            sourceTenderId2 = t2.getPublications().stream().filter(Objects::nonNull)
                    .filter(p -> p.getIsIncluded() != null).filter(Publication::getIsIncluded)
                    .map(Publication::getSourceTenderId).filter(Objects::nonNull).findFirst().orElse(null);
        }

        double similarity = 0.0;

        // buyers
        if (buyersGroupIds1 != null && !buyersGroupIds1.isEmpty() && buyersGroupIds2 != null && !buyersGroupIds2.isEmpty()) {
            // buyers group ids
            if (buyersGroupIds1.equals(buyersGroupIds2)) {
                similarity += 1;
            } else if (!Sets.intersection(new HashSet<>(buyersGroupIds1), new HashSet<>(buyersGroupIds2)).isEmpty()) {
                similarity += 0.5;
            }
        } else {
            // if group ids are different, then compare buyer names
            if (buyersNames1 != null && !buyersNames1.isEmpty() && t2.getBuyers() != null) {
                t2.setBuyers(getPopulatedBuyers(t2));
                buyersNames2 = t2.getBuyers().stream().filter(Objects::nonNull)
                        .map(MatchedBody::getName).filter(Objects::nonNull).collect(Collectors.toList());
                if (buyersNames1.equals(buyersNames2)) {
                    similarity += 1;
                } else if (buyersNames1.size() == buyersNames2.size()) {
                    double namesSimilarity = 1;
                    for (int i = 0; i < buyersNames1.size(); i++) {
                        namesSimilarity *= computeTrigramSimilarity(buyersNames1.get(i), buyersNames2.get(i));
                    }
                    if (namesSimilarity > 0.5) {
                        similarity += namesSimilarity;
                    }
                }
            } else {
                similarity += 0.5;
            }
        }

        // title
        if (title1 != null && !title1.isEmpty() && title2 != null && !title2.isEmpty()) {
            similarity += computeTrigramSimilarity(title1, title2);
        } else {
            similarity += 0.5;
        }

        // sourceTenderId
        if (sourceTenderId1 != null && !sourceTenderId1.isEmpty() && sourceTenderId2 != null && !sourceTenderId2.isEmpty()) {
            similarity += computeTrigramSimilarity(sourceTenderId1, sourceTenderId2);
        } else {
            similarity += 0.5;
        }
        return similarity;
    }


    /**
     * Cleans and computes trigram similarity for two strings.
     * @param s1 string 1
     * @param s2 string 2
     * @return similarity
     */
    private double computeTrigramSimilarity(final String s1, final String s2) {
        return new NGramDistance(3).getDistance(
                DigestUtils.removeAccents(s1).toLowerCase(),
                DigestUtils.removeAccents(s2).toLowerCase());
    }


    @Override
    public final boolean isMatchable(final MatchedTender item) {
        return item.getBuyerAssignedId() != null && item.getBuyers() != null
                && !item.getBuyers().isEmpty() && item.getTitle() != null;
    }


}
