package eu.dl.worker.matched.plugin;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collector;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.dataaccess.dao.PoolBodyDAO;
import eu.dl.dataaccess.dto.matched.EtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedGroupInfo;
import eu.dl.dataaccess.dto.matched.PoolBody;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Base class for plugin that attempts to find match with bodies from the pool. 
 * 
 * @param <T>
 *      class of the matched body
 * @param <U>
 *      class of the item from body pool
 * @param <V>
 *      class of the pool body DAO
 */
public abstract class BaseBodiesPoolMatchingPlugin<T extends MatchedBody, U extends PoolBody, V extends PoolBodyDAO>
    implements MatchingPlugin<T> {

    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    protected final V poolDAO;

    /**
     * Constructor with body pool DAO initialization.
     *
     * @param poolDAO
     *      body pool DAO
     */
    public BaseBodiesPoolMatchingPlugin(final V poolDAO) {
        this.poolDAO = poolDAO;
    }

    @Override
    public final MatchingResult match(final T item) {
        final MatchingResult matchingResult = new MatchingResult();
        
        final List<U> matchedPoolBodies = getBodiesPool(item);

        logger.info("Body pool includes {} items", matchedPoolBodies.size());

        if (!matchedPoolBodies.isEmpty()) {
            Map<U, Float> similarities = getSimiliraties(item, matchedPoolBodies);

            if (!similarities.isEmpty()) {
                final Map.Entry<U, Float> best = getBest(similarities);
                MatchedBody bestMatchedBody = bestToMatchedBody(best.getKey());

                HashMap<String, Object> meta = new HashMap<>();
                meta.put("matchingScore", best.getValue());
                meta.put("pairedBodyId", bestMatchedBody.getId());

                matchingResult.setGroupId(bestMatchedBody.getGroupId());
                matchingResult.setMatched(true);
                matchingResult.setMatchedBy(getMatchedBy());
                matchingResult.setMetaData(meta);
                matchingResult.setMatchedBody(bestMatchedBody);
            }
        }

        return matchingResult;
    }

    /**
     * @see BaseBodiesPoolMatchingPlugin#getBest(java.util.Map) 
     * @param best
     *      best match
     * @return best match as MatchedBody with not null id and groupId
     */
    protected abstract MatchedBody bestToMatchedBody(U best);

    /**
     * @param item
     *      body to be matched
     * @param pool
     *      bodies pool for matching
     * @return similarities of item with each body from pool
     */
    protected abstract Map<U, Float> getSimiliraties(T item, List<U> pool);

    /**
     * @return plugin identifier
     */
    protected abstract String getMatchedBy();
    
    /**
     * Returns the pool of bodies for matching of the given {@code item}.
     * 
     * @param item
     *      macthed item
     * @return list of bodies from pool
     */
    protected abstract List<U> getBodiesPool(T item);
        
    /**
     * @see #getSimiliraties(eu.dl.dataaccess.dto.matched.MatchedBody, java.util.List)
     *
     * @param similarities
     *      similarities
     * @return best similarity
     */
    private Map.Entry<U, Float> getBest(final Map<U, Float> similarities) {
        if (similarities == null) {
            return null;
        }

        List<Map.Entry<U, Float>> max = similarities.entrySet().stream()
            .collect(ArrayUtils.max((a, b) -> Float.compare(a.getValue(), b.getValue())));

        if (max.size() == 1) {
            return max.get(0);
        } else {
            final Map<String, List<Map.Entry<U, Float>>> byGroups = new HashMap<>();
            max.forEach(n -> {
                String gid = n.getKey().getGroupId();

                if (!byGroups.containsKey(gid)) {
                    byGroups.put(gid, new ArrayList<>());
                }
                byGroups.get(gid).add(n);
            });

            // if only one group exists, return first max
            if (byGroups.size() == 1) {
                return max.get(0);
            }

            List<MatchedGroupInfo> info = poolDAO.getGroupsInfo(Arrays.asList(byGroups.keySet().toArray()));

            List<MatchedGroupInfo> bestGroups = getGroupsWithHighestScore(info);

            // if pool body isn't instance of EtalonBody log error that exist more than one matched group with the
            // etalon. For etalons it is expected result, each etalon has own group.
            if (!(max.get(0).getKey() instanceof EtalonBody)
                && bestGroups.stream().anyMatch(n -> n.getHasEtalon()) && bestGroups.size() > 1) {

                StringBuilder groupIds = new StringBuilder();
                for (MatchedGroupInfo m : bestGroups) {
                    if (groupIds.length() > 0) {
                        groupIds.append(", ");
                    }
                    groupIds.append(m.getGroupId());
                }

                logger.error("Number of matched groups whitch includes etalon is greather then 1. {}", groupIds);
            }

            return byGroups.get(bestGroups.get(0).getGroupId()).get(0);
        }
    }

    /**
     * @see PoolBodyDAO#getGroupsInfo(java.util.List)
     * @see #groupsWithHighestScore()
     *
     * @param groupsInfo
     *      list of informations about groups
     * @return list of groups with highest score or empty list
     */
    protected final List<MatchedGroupInfo> getGroupsWithHighestScore(final List<MatchedGroupInfo> groupsInfo) {
        if (groupsInfo == null) {
            return Collections.emptyList();
        }

        return groupsInfo.stream().collect(groupsWithHighestScore());
    }

    /**
     * Collects all best groups. Groups with etalon has highest priority so doesn't return combination of etalon and
     * non-etalon groups. Better group is group with highest number of records.
     *
     * @return collector that collects best groups
     */
    protected final Collector<MatchedGroupInfo, ?, List<MatchedGroupInfo>> groupsWithHighestScore() {
        return Collector.of(
            ArrayList::new,
            (list, t) -> {
                if (list.isEmpty()) {
                    list.add(t);
                    return;
                }

                MatchedGroupInfo current = list.get(0);
                // new maximum - in case that tested group includes etalon and current maximum groups not or if size of
                // tested group is bigger then size of current maximal groups.
                if ((!current.getHasEtalon() && t.getHasEtalon()) || (current.getSize() < t.getSize())) {
                    list.clear();
                    list.add(t);
                // add new entry to the current maxes list - in case that tested and current item has same values in
                // parameters hasEtalon and size.
                } else if (Objects.equals(current.getHasEtalon(), t.getHasEtalon())
                    && Objects.equals(current.getSize(), t.getSize())) {
                    list.add(t);
                }
            },
            (list1, list2) -> {
                if (list1.isEmpty()) {
                    return list2;
                }
                if (list2.isEmpty()) {
                    return list1;
                }

                MatchedGroupInfo a = list1.get(0);
                MatchedGroupInfo b = list2.get(0);

                if (!a.getHasEtalon() && b.getHasEtalon()) {
                    return list2;
                } else if (a.getHasEtalon() && !b.getHasEtalon()) {
                    return list1;
                } else if (a.getSize() > b.getSize()) {
                    return list1;
                } else if (a.getSize() < b.getSize()) {
                    return list2;
                } else {
                    list1.addAll(list2);
                    return list1;
                }
            });
    }
}
