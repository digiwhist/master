package eu.dl.worker.matched;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.ManualMatchDAO;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.utils.BodyUtils;
import eu.dl.dataaccess.utils.WeightedHash;
import eu.dl.core.cache.Cache;
import eu.dl.core.cache.CacheFactory;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.matched.plugin.ApproximateMatchingEtalonPlugin;
import eu.dl.worker.matched.plugin.ApproximateMatchingPlugin;
import eu.dl.worker.matched.plugin.ExactMatchingEtalonPlugin;
import eu.dl.worker.matched.plugin.ExactMatchingPlugin;
import eu.dl.worker.matched.plugin.ManualMatchingPlugin;
import eu.dl.worker.matched.plugin.MatchingPlugin;
import eu.dl.worker.matched.plugin.MatchingResult;
import eu.dl.worker.utils.BasicPluginRegistry;
import eu.dl.worker.utils.PluginRegistry;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.ThreadContext;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import static eu.dl.dataaccess.utils.DigestUtils.bodyFullHash;
import static eu.dl.dataaccess.utils.DigestUtils.generateAlternativeBodyHashes;

/**
 * Base class for tender matchers. Waits for clean tender data, matches each
 * sub-part and stores result to db.
 */
public abstract class BaseTenderMatcher extends BaseMatcher {

    private final CleanTenderDAO<CleanTender> cleanDao = getCleanTenderDAO();

    protected MatchedBodyDAO matchedBodyDao;

    protected MatchedTenderDAO matchedTenderDao;

    protected final ManualMatchDAO manualMatchDao = getManualMatchDAO();

    protected final EtalonBodyDAO etalonBodyDao = getEtalonBodyDAO();

    protected PluginRegistry<MatchingPlugin<MatchedBody>> bodyPluginRegistry = new
            BasicPluginRegistry<MatchingPlugin<MatchedBody>>();

    protected PluginRegistry<MatchingPlugin<MatchedTender>> tenderPluginRegistry = new
            BasicPluginRegistry<MatchingPlugin<MatchedTender>>();

    /**
     * String for body that was matched by hash.
     */
    public static final String HASH = "hash";

    /**
     * String for unmatched body. Indicates that no plugin was successful
     */
    public static final String UNMATCHED = "unmatched";

    protected static final String MANUAL_PLUGIN = "manual";

    protected static final String EXACT_MATCH_BODY_PLUGIN = "bodyExact";
    protected static final String EXACT_MATCH_ETALON_PLUGIN = "etalonExact";
    protected static final String APPROXIMATE_MATCH_BODY_PLUGIN = "bodyApproximate";
    protected static final String APPROXIMATE_MATCH_ETALON_PLUGIN = "etalonApproximate";

    /**
     * Tag suffix used to send messages about body matching.
     */
    public static final String BODY_MESSAGING_TAG_SUFFIX = "_body";

    /**
     * Tag suffix used to send messages about tender matching.
     */
    public static final String TENDER_MESSAGING_TAG_SUFFIX = "_tender";

    /**
     * Time execution threshold of match/master plugins (in milliseconds).
     */
    private static final long PLUGIN_TIME_THRESHOLD = 100;

    private final ManualMatchingPlugin<MatchedBody> manualBodyMatchingPlugin;

    protected Cache hashCache;

    private Boolean cacheEnabled = false;

    private String hashCachePrefix = "hash::";

    /**
     * Default constructor.
     */
    protected BaseTenderMatcher() {
        this(null);
    }

    /**
     * Constructor that takes list of additional matchers.
     *
     * @param additionalMatchers
     *         list of matchers (worker names and versions) whose data should be included in searching for a match
     */
    protected BaseTenderMatcher(final List<Pair<String, String>> additionalMatchers) {
        super();

        matchedBodyDao = getMatchedBodyDAO(additionalMatchers);
        matchedTenderDao = getMatchedTenderDAO(additionalMatchers);

        registerCommonBodyPlugins();
        registerBodyPlugins();

        registerCommonTenderPlugins();
        registerTenderPlugins();

        if (config.getParam("cache.enabled") != null && config.getParam("cache.enabled").equals("true")) {
            this.cacheEnabled = true;
            hashCache = CacheFactory.getCache(this.getClass().getSimpleName().concat("::"));

            String repopulate = hashCache.get(hashCachePrefix.concat("repopulate"));

            if (repopulate == null || repopulate.equals("true")) {
                populateBodyHashCache();
                populateEtalonCache();
                hashCache.put(hashCachePrefix.concat("repopulate"), "false");
            }
        }

        manualBodyMatchingPlugin = new ManualMatchingPlugin<MatchedBody>(manualMatchDao, "body");
    }

	@Override
    public final void doWork(final Message message) {
        getTransactionUtils().begin();
        final String cleanTenderId = message.getValue("id");
        ThreadContext.put("clean_tender_id", cleanTenderId);
        CleanTender cleanTender = cleanDao.getById(cleanTenderId);

        logger.info("Matching bodies for tender {}", cleanTenderId);
        MatchedTender matchedTender = new MatchedTender(cleanTender);
        matchedTender.setPersistentId(cleanTender.getPersistentId());
        matchedTender.setCreatedRaw(cleanTender.getCreatedRaw());
        // set item processing order
        matchedTender.setProcessingOrder(cleanTender.getProcessingOrder());

        matchedTender = matchBodies(matchedTender, cleanTender);

        logger.info("Matching tender {}", cleanTenderId);
        matchTender(matchedTender);
        getTransactionUtils().commit();
    }

    /**
     * Body matching.
     *
     * @param matchedTender
     *         matched tender
     *
     * @param cleanTender
     *         clean tender
     *
     * @return tender with matched bodies set
     */
    private MatchedTender matchBodies(final MatchedTender matchedTender, final CleanTender cleanTender) {
        LocalDate publicationDate = null;
        String source = null;
        if (matchedTender.getPublications() != null) {
            for (Publication publication : matchedTender.getPublications()) {
                if (publication != null && publication.getIsIncluded() != null && publication.getIsIncluded()) {
                    publicationDate = publication.getPublicationDate();
                    source = publication.getSource().toString();
                }
            }
        }
        matchedTender.setAdministrators(
                matchBodySet(matchedTender.getAdministrators(), cleanTender, publicationDate, source));
        matchedTender.setApproachedBidders(
                matchBodySet(matchedTender.getApproachedBidders(), cleanTender, publicationDate, source));
        matchedTender.setCandidates(
                matchBodySet(matchedTender.getCandidates(), cleanTender, publicationDate, source));
        matchedTender.setSupervisors(
                matchBodySet(matchedTender.getSupervisors(), cleanTender, publicationDate, source));
        matchedTender.setBuyers(matchBodySet(matchedTender.getBuyers(), cleanTender, publicationDate, source));

        matchedTender.setOnBehalfOf(
                matchBodySet(matchedTender.getOnBehalfOf(), cleanTender, publicationDate, source));
        matchedTender.setBidsRecipient(
                matchBody(matchedTender.getBidsRecipient(), cleanTender, publicationDate, source));
        matchedTender.setFurtherInformationProvider(
                matchBody(matchedTender.getFurtherInformationProvider(), cleanTender, publicationDate, source));
        matchedTender.setSpecificationsCreator(
                matchBody(matchedTender.getSpecificationsCreator(), cleanTender, publicationDate, source));
        matchedTender.setSpecificationsProvider(
                matchBody(matchedTender.getSpecificationsProvider(), cleanTender, publicationDate, source));

        List<MatchedTenderLot> lots = matchedTender.getLots();
        if (lots != null) {
            for (MatchedTenderLot lot : lots) {
                List<MatchedBid> bids = lot.getBids();
                if (bids != null) {
                    for (MatchedBid bid : bids) {
                        bid.setBidders(matchBodySet(bid.getBidders(), cleanTender, publicationDate, source));
                        bid.setSubcontractors(
                                matchBodySet(bid.getSubcontractors(), cleanTender, publicationDate, source));
                    }
                }
                lot.setBids(bids);
            }
            matchedTender.setLots(lots);
        }
        return matchedTender;
    }

    /**
     * Matches one body.
     *
     * @param body
     *         body to be matched
     * @param cleanTender
     *         source of the matched body
     * @param publicationDate
     *         publication date
     * @param source
     *         body source
     *
     * @return list of matched bodies
     */
    private MatchedBody matchBody(final MatchedBody body, final CleanTender cleanTender,
            final LocalDate publicationDate, final String source) {
        if (body == null) {
            return null;
        }

        List<MatchedBody> matchedBodies = matchBodySet(Arrays.asList(body), cleanTender, publicationDate, source);

        if (matchedBodies != null && !matchedBodies.isEmpty()) {
            return matchedBodies.get(0);
        }
        return null;
    }

    /**
     * Matches body set. Doesn't return the whole body set, only the "plain"
     * bodies with id and groupId set
     *
     * @param bodies
     *         bodies to be matched
     * @param cleanTender
     *         source of the body
     * @param publicationDate
     *         publication date
     * @param source
     *         body source
     *
     * @return list of "plain/reference" bodies
     */
    private List<MatchedBody> matchBodySet(final List<MatchedBody> bodies, final CleanTender cleanTender,
            final LocalDate publicationDate, final String source) {
        if (bodies == null) {
            return null;
        }

        List<MatchedBody> matchedBodies = new ArrayList<MatchedBody>();
        // got through all bodies available and try to match them
        for (MatchedBody body : bodies) {
            if (body == null) {
                continue;
            }

            generateHashes(body);

            if (body.getHash() == null) {
                continue;
            }

            HashMap<String, Object> metaData = new HashMap<String, Object>();
            if (body.getMetaData() != null) {
                metaData = body.getMetaData();
            }

            // search by manual matching plugin first
            long pluginStartTime = System.currentTimeMillis();
            MatchingResult manualMatchingResult = manualBodyMatchingPlugin.match(body);
            long pluginEndTime = System.currentTimeMillis();

            logMatchingData(pluginStartTime, pluginEndTime, "manual");

            HashMap<String, Long> matchingTimes = new HashMap<String, Long>();
            matchingTimes.put("manual", pluginEndTime - pluginStartTime);

            if (manualMatchingResult.getMatched()) {
                // match found, store group_id and matched by
                body.setGroupId(manualMatchingResult.getGroupId());
                body.setMatchedBy(manualMatchingResult.getMatchedBy());
                metaData.put("matchedBy", manualMatchingResult.getMatchedBy());
                metaData.put("matchingData", manualMatchingResult.getMetaData());
            } else {

                String matchedByHashGroupId = null;
                if (this.cacheEnabled) {
                    // search for potential matches in cache
                    pluginStartTime = System.currentTimeMillis();
                    matchedByHashGroupId = findByHashes(body);
                    pluginEndTime = System.currentTimeMillis();

                    logMatchingData(pluginStartTime, pluginEndTime, "HASH");

                    matchingTimes.put("HASH", pluginEndTime - pluginStartTime);
                }

                if (matchedByHashGroupId != null) {
                    // the same hash found, storing into the same group
                    body.setGroupId(matchedByHashGroupId);
                    body.setMatchedBy(HASH);
                    metaData.put("matchedBy", HASH);
                } else {
                    Boolean matched = false;
                    // try all registered plugins for potential match
                    for (Entry<String, MatchingPlugin<MatchedBody>> entry : bodyPluginRegistry.getPlugins().entrySet()) {
                        MatchingPlugin<MatchedBody> plugin = entry.getValue();
                        pluginStartTime = System.currentTimeMillis();
                        MatchingResult matchingResult = plugin.match(body);
                        pluginEndTime = System.currentTimeMillis();
                        logMatchingData(pluginStartTime, pluginEndTime, plugin.getClass().getName());
                        matchingTimes.put(plugin.getClass().getName(), pluginEndTime - pluginStartTime);

                        if (matchingResult.getMatched()) {
                            // match found, store group_id and matched by
                            body.setGroupId(matchingResult.getGroupId());
                            body.setMatchedBy(matchingResult.getMatchedBy());
                            metaData.put("matchedBy", matchingResult.getMatchedBy());
                            metaData.put("matchingData", matchingResult.getMetaData());
                            matched = true;

                            if (this.cacheEnabled) {
                                // save results to cache
                                if (matchingResult.getMatchedBy().equals(ExactMatchingEtalonPlugin.MATCHED_BY)
                                        || matchingResult.getMatchedBy().equals(ApproximateMatchingEtalonPlugin.MATCHED_BY)) {
                                    hashCache.put(hashCachePrefix.concat(matchingResult.getGroupId()), "true");
                                    putToCache(hashCachePrefix.concat(matchingResult.getGroupId()), matchingResult.getMatchedBody());
                                } else {
                                    hashCache.put(matchingResult.getGroupId(), "false");
                                }
                            }

                            // end the plugin loop
                            break;
                        }
                    }

                    // not matched by any of our plugins, store as a new item
                    if (!matched) {
                        body.setGroupId("group_" + getSourceId() + "_body_" + body.getHash());
                        metaData.put("matchedBy", UNMATCHED);
                    }
                }
            }

            logger.debug("Body with hash '{}' matched by '{}' with group id '{}'", body.getHash(), body.getMatchedBy(),
                    body.getGroupId());

            if (this.cacheEnabled) {
                putToCache(body.getGroupId(), body);
            }

            // save the result
            body.setCleanObjectId(cleanTender.getId());
            body.setProcessingOrder(cleanTender.getProcessingOrder());
            body.setPublicationDate(publicationDate);
            body.setSource(source);
            body.setRawObjectId(cleanTender.getRawObjectId());
            Double completenessScore = BodyUtils.completenessScore(body);
            metaData.put("completenessScore", completenessScore);
            metaData.put("cleanObjectPersistentId", cleanTender.getPersistentId());
            metaData.put("matchingTimes", matchingTimes);

            body.setMetaData(metaData);
            matchedBodyDao.save(body);

            // we don't store the whole body, only the "plain", reference body
            MatchedBody plainBody = new MatchedBody();
            plainBody.setId(body.getId());
            plainBody.setGroupId(body.getGroupId());
            plainBody.setCompletenessScore(completenessScore);

            matchedBodies.add(plainBody);

            // publish message
            final Message outgoingMessage = MessageFactory.getMessage();
            final String tag = getBodyMessagingTag();
            outgoingMessage.setValue("groupId", body.getGroupId());
            publishMessage(outgoingMessage, tag);
            logger.info("Body matching finished, published message '{}' with tag '{}'", outgoingMessage, tag);
        }

        if (matchedBodies != null && !matchedBodies.isEmpty()) {
            return matchedBodies;
        }

        return null;
    }


    /**
     * Generate body hashes.
     *
     * @param body hashes to be generated
     */
    private void generateHashes(final MatchedBody body) {
        // generate body hash
        String mainBodyHash = generateBodyHash(body);

        if (mainBodyHash == null) {
            logger.error("Unable to generate body hash. Body is skipped.");
            return;
        }

        logger.debug("Calculated hash {} for body {}.", mainBodyHash, body.getName());
        body.setHash(mainBodyHash);

        String fullHash = generateBodyFullHash(body);
        logger.debug("Calculated full hash {} for body {}.", fullHash, body.getName());
        body.setFullHash(fullHash);
        body.setAlternativeHashes(generateAlternativeBodyHashes(body));
    }

    /**
     * Puts values with matching result to cache.
     *
     * @param groupId the group id where the body was stored
     * @param body matched body
     */
    private void putToCache(final String groupId, final MatchedBody body) {
    	if (body.getHash() != null) {
    		hashCache.put(hashCachePrefix.concat(body.getHash()), groupId);
    	}

        if (body.getFullHash() != null) {
            hashCache.put(hashCachePrefix.concat(body.getFullHash()), groupId);
        }

    	for (WeightedHash hash : body.getAlternativeHashes()) {
    		hashCache.put(hashCachePrefix.concat(hash.getHash()), groupId);
    	}
	}

	/**
     * This method searches by body hashes for potential matches.
     *
     * @param body for which body to search
     * @return groupid or null if nothing found
     */
    private String findByHashes(final MatchedBody body) {
    		// check first, whether there is not the "same"(in the sense of the equal hash) body
        String groupId = hashCache.get(hashCachePrefix.concat(body.getHash()));
        if (groupId != null) {
        		return groupId;
    		}

        String winningGroupId = null;
        Double winnerWeight = null;

        List<WeightedHash> alternativeHashes = body.getAlternativeHashes();

        alternativeHashes.sort(Comparator.comparing(WeightedHash::getWeight).reversed()
                .thenComparing(Comparator.comparing(WeightedHash::getHash)));

        for (WeightedHash hash : alternativeHashes) {
            groupId = hashCache.get(hashCachePrefix.concat(hash.getHash()));

            if (groupId != null) {
            		String isEtalonGroup = hashCache.get(hashCachePrefix.concat(groupId));

            		if (isEtalonGroup != null && isEtalonGroup.equals("true")) {
    					// etalon wins, no need to wait
    					return groupId;
            		}

            		if (winningGroupId == null) {
            			// first go through, init data
        				winningGroupId = groupId;
        				winnerWeight = hash.getWeight();
        			} else {
        				// next round, compare weight with previous
        				if ((winnerWeight - hash.getWeight()) > 0.9) {
        					// less specific hash, return previous one
        					return winningGroupId;
        				}
        			}
        		}
        }

        return winningGroupId;
	}

	/**
     * Body matching.
     *
     * @param matchedTender
     *         matched tender
     */
    private void matchTender(final MatchedTender matchedTender) {

        // generate body hash
        String hash = generateTenderHash(matchedTender);
        matchedTender.setHash(hash);

        logger.debug("Calculated hash '{}' for clean tender '{}' ", hash, matchedTender.getId());
        // check first, whether there is not the "same"(in the sense of the
        // equal hash) body
        List<MatchedTender> sameHash = matchedTenderDao.getByHash(hash);
        if (!sameHash.isEmpty()) {
            // the same hash found, storing into the same group
            matchedTender.setGroupId(sameHash.get(0).getGroupId());
            matchedTender.setMatchedBy(HASH);
        } else {
            Boolean matched = false;
            // try all registered plugins for potential match
            for (Entry<String, MatchingPlugin<MatchedTender>> entry : tenderPluginRegistry.getPlugins().entrySet()) {
                MatchingPlugin<MatchedTender> plugin = entry.getValue();
                long pluginStartTime = System.currentTimeMillis();
                MatchingResult matchingResult = plugin.match(matchedTender);
                long pluginEndTime = System.currentTimeMillis();
                if ((pluginEndTime - pluginStartTime) > PLUGIN_TIME_THRESHOLD) {
                    logger.warn("Execution of tender match plugin {} took {} ms.", plugin,
                            pluginEndTime - pluginStartTime);
                }

                if (matchingResult.getMatched()) {
                    // match found, store group_id and matched by
                    matchedTender.setGroupId(matchingResult.getGroupId());
                    matchedTender.setMatchedBy(matchingResult.getMatchedBy());
                    matched = true;

                    // end the plugin loop
                    break;
                }
            }

            // not matched by any of our plugins, store as a new item
            if (!matched) {
                matchedTender.setGroupId("group_" + getSourceId() + "_tender_" + hash);
                matchedTender.setMatchedBy(UNMATCHED);
            }
        }
        logger.debug("Tender with hash '{}' matched by '{}' with group id '{}'", matchedTender.getHash(),
                matchedTender.getMatchedBy(), matchedTender.getGroupId());

        // save the result
        matchedTenderDao.save(matchedTender);

        final Message outgoingMessage = MessageFactory.getMessage();
        final String tag = getTenderMessagingTag();
        outgoingMessage.setValue("groupId", matchedTender.getGroupId());
        publishMessage(outgoingMessage, tag);
        logger.info("Tender matching finished, published message '{}' with tag '{}'", outgoingMessage, tag);
    }

    @Override
    protected final void resend(final String version, final String dateFrom, final String dateTo) {
        logger.debug("Resending messages to be mastered.");

        try {
            String resendVersion = version;
            if (version.equals(LATEST)) {
                // current version data should be resent
                resendVersion = getVersion();
            }

            final List<MatchedTender> tenders = matchedTenderDao.getForResend(getName(), resendVersion);

            for (final MatchedTender tender : tenders) {
                final Message outgoingMessage = MessageFactory.getMessage();
                final String tag = getTenderMessagingTag();
                outgoingMessage.setValue("groupId", tender.getGroupId());
                publishMessage(outgoingMessage, tag);
            }
            logger.info("Messages with matched tenders sent over.");

            final List<MatchedBody> bodies = matchedBodyDao.getForResend(getName(), resendVersion);
            for (MatchedBody body : bodies) {
                final Message outgoingMessage = MessageFactory.getMessage();
                final String tag = getBodyMessagingTag();
                outgoingMessage.setValue("groupId", body.getGroupId());
                publishMessage(outgoingMessage, tag);
            }
            logger.info("Messages with matched bodies sent over.");
        } catch (final Exception ex) {
            logger.error("Unable to resend messages for matching {}", ex);
            throw new UnrecoverableException("Unable to resend messages for matching", ex);
        }
    }

    /**
     * Registration of plugin used to clean all bodies.
     */
    private void registerCommonBodyPlugins() {
        bodyPluginRegistry.registerPlugin(EXACT_MATCH_ETALON_PLUGIN,
                new ExactMatchingEtalonPlugin(matchedBodyDao, etalonBodyDao, getSourceId()));

        bodyPluginRegistry.registerPlugin(EXACT_MATCH_BODY_PLUGIN, new ExactMatchingPlugin(matchedBodyDao));

        bodyPluginRegistry.registerPlugin(APPROXIMATE_MATCH_BODY_PLUGIN, new ApproximateMatchingPlugin(matchedBodyDao));

        bodyPluginRegistry.registerPlugin(APPROXIMATE_MATCH_ETALON_PLUGIN,
                new ApproximateMatchingEtalonPlugin(matchedBodyDao, etalonBodyDao, getSourceId()));
    }

    /**
     * Registration of plugin used to clean the tender.
     */
    private void registerCommonTenderPlugins() {
        tenderPluginRegistry.registerPlugin(MANUAL_PLUGIN,
                new ManualMatchingPlugin<MatchedTender>(manualMatchDao, "tender"));
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    /**
     * Registration of plugin used to clean the tender.
     */
    protected abstract void registerTenderPlugins();

    /**
     * Plugins for body cleaning are registered in this method.
     */
    protected abstract void registerBodyPlugins();

    /**
     * This methods generates the hash for the body. This hash is used to group
     * the same bodies together. The bodies with the same hash are not
     * necessarily "equal"/"the same" in all their fields - only the relevant
     * fields counts. For example name and ICO.
     *
     * @param matchedBody
     *         matched body used to generate hash
     *
     * @return hash
     */
    protected abstract String generateBodyHash(MatchedBody matchedBody);

    /**
     * This methods generates the hash for the tender. This hash is used to
     * group the same tenders together. The tenders with the same hash are not
     * necessarily "equal"/"the same" in all their fields - only the relevant
     * fields counts. For example title, date, price.
     *
     * @param matchedTender
     *         matched tender used to generate hash
     *
     * @return hash
     */
    protected abstract String generateTenderHash(MatchedTender matchedTender);

    /**
     * Provides DAO to implement clean tenders.
     *
     * @return clean tender DAO
     */
    protected abstract CleanTenderDAO getCleanTenderDAO();

    /**
     * Provides DAO to handle matched tenders.
     *
     * @param additionalMatchers
     *         list of matchers (worker names and versions) whose data should be included in searching for a match (for
     *         cross-source matching purposes)
     *
     * @return matched tender DAO
     */
    protected abstract MatchedTenderDAO getMatchedTenderDAO(List<Pair<String, String>> additionalMatchers);

    /**
     * Provides DAO to handle matched bodies.
     *
     * @param additionalMatchers
     *         list of matchers (worker names and versions) whose data should be included in searching for a match (for
     *         cross-source matching purposes)
     *
     * @return matched body DAO
     */
    protected abstract MatchedBodyDAO getMatchedBodyDAO(List<Pair<String, String>> additionalMatchers);

    /**
     * Provides DAO to handle manual matches.
     *
     * @return manual match DAO
     */
    protected abstract ManualMatchDAO getManualMatchDAO();

    /**
     * Tag used for messages about finished tender matching.
     *
     * @return tag
     */
    protected abstract String getTenderMessagingTag();

    /**
     * Tag used for messages about finished body matching.
     *
     * @return tag
     */
    protected abstract String getBodyMessagingTag();

    /**
     * Provides DAO to handle matches with etalon.
     *
     * @return etalon body DAO
     */
    protected abstract EtalonBodyDAO getEtalonBodyDAO();

    /**
     * This methods generates the full hash for the body.
     *
     * @param matchedBody
     *         matched body used to generate full hash
     *
     * @return body full hash
     */
    protected final String generateBodyFullHash(final MatchedBody matchedBody) {
        return bodyFullHash(matchedBody);
    }

    /**
     * Populates body hash cache with already matched results.
     */
    private void populateBodyHashCache() {
        logger.info("Populating body hash cache.");
		Map<String, String> hashes = matchedBodyDao.getHashAndGroupIds();
		for (Map.Entry<String, String> entry : hashes.entrySet()) {
			String value = hashCache.get(hashCachePrefix.concat(entry.getKey()));
			if (value == null) {
				hashCache.put(hashCachePrefix.concat(entry.getKey()), entry.getValue());
			}
		}
		logger.info("Body hash cache populated.");
	}

    /**
     * Populates body hash cache with already matched results.
     */
    private void populateEtalonCache() {
        logger.info("Populating etalon cache.");
        List<String> list = matchedBodyDao.getEtalonGroupIds();
		for (String groupId : list) {
			hashCache.put(hashCachePrefix.concat(groupId), "true");
		}
		logger.info("Etalon cache populated.");
	}

    /**
     * Logs about matching to thread context.
     *
     * @param pluginStartTime start time
     * @param pluginEndTime end time
     * @param pluginName plugin name
     */
	private void logMatchingData(final long pluginStartTime, final long pluginEndTime, final String pluginName) {
        long executionTime = pluginEndTime - pluginStartTime;
        ThreadContext.put("matching_time", new Long(executionTime).toString());
        ThreadContext.put("matching_plugin_name", "manual");
        logger.error("Execution of body match plugin {} took {} ms.", pluginName, executionTime);
    }
}
