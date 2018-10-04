package eu.dl.dataaccess.dto.utils;

import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.OCDSAwardCriteria;
import eu.dl.dataaccess.dto.codetables.OCDSBidStatisticMeasure;
import eu.dl.dataaccess.dto.codetables.OCDSDocumentType;
import eu.dl.dataaccess.dto.codetables.OCDSInitiationType;
import eu.dl.dataaccess.dto.codetables.OCDSPartyRole;
import eu.dl.dataaccess.dto.codetables.OCDSProcedureMethod;
import eu.dl.dataaccess.dto.codetables.OCDSProcurementCategory;
import eu.dl.dataaccess.dto.codetables.OCDSReleaseTag;
import eu.dl.dataaccess.dto.codetables.OCDSSubmissionMethod;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Document;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.ocds.OCDSAddress;
import eu.dl.dataaccess.dto.ocds.OCDSAward;
import eu.dl.dataaccess.dto.ocds.OCDSBid;
import eu.dl.dataaccess.dto.ocds.OCDSBidDetail;
import eu.dl.dataaccess.dto.ocds.OCDSBidStatistic;
import eu.dl.dataaccess.dto.ocds.OCDSContactPoint;
import eu.dl.dataaccess.dto.ocds.OCDSContract;
import eu.dl.dataaccess.dto.ocds.OCDSDocument;
import eu.dl.dataaccess.dto.ocds.OCDSIdentifier;
import eu.dl.dataaccess.dto.ocds.OCDSImplementation;
import eu.dl.dataaccess.dto.ocds.OCDSItem;
import eu.dl.dataaccess.dto.ocds.OCDSItemClassification;
import eu.dl.dataaccess.dto.ocds.OCDSLot;
import eu.dl.dataaccess.dto.ocds.OCDSOrganization;
import eu.dl.dataaccess.dto.ocds.OCDSOrganizationReference;
import eu.dl.dataaccess.dto.ocds.OCDSPeriod;
import eu.dl.dataaccess.dto.ocds.OCDSPublisher;
import eu.dl.dataaccess.dto.ocds.OCDSRelease;
import eu.dl.dataaccess.dto.ocds.OCDSReleasePackage;
import eu.dl.dataaccess.dto.ocds.OCDSTender;
import eu.dl.dataaccess.dto.ocds.OCDSTransaction;
import eu.dl.dataaccess.dto.ocds.OCDSValue;
import eu.dl.dataaccess.utils.ClassUtils;
import java.math.BigDecimal;
import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class holds useful methods for transmutation of MasterTender to OCDSRelease.
 *
 * @author Tomas Mrazek
 */
public final class OCDSUtils {

    private static final Logger logger = LoggerFactory.getLogger(OCDSUtils.class);

    /**
     * Source of indexes for OCDSUtils.
     */
    enum Index {
        DEFAULT,
        LOT,
        BID,
        AWARD,
        DOCUMENT,
        ITEM,
        STATISTIC,
        CONTRACT,
        TRANSACTION;

        private int index = 0;

        /**
         * @return next value
         */
        public String next() {
            return getIndex(++index);
        }

        /**
         * @return current value of index
         */
        public String current() {
            return getIndex(index);
        }

        /**
         * Resets the index.
         */
        public void reset() {
            index = 0;
        }

        /**
         * @param idx
         *      index
         * @return return index string representation
         */
        private String getIndex(final int idx) {
            return this.equals(DEFAULT) ? "" : this.name().toLowerCase() + "-" + idx;
        }

        /**
         * Resets the given indexes.
         *
         * @param index
         *      one or more indexes to be reset
         */
        public static void reset(final Index... index) {
            for (Index i : index) {
                i.reset();
            }
        }
        
        /**
         * Resets all indexes.
         */
        public static void resetAll() {
            reset(Index.values());
        }
    }

    /**
     * Suppress default constructor for noninstantiability.
     */
    private OCDSUtils() {
        throw new AssertionError();
    }

    /**
     * Gets OCDS Organization from MasterBody.
     *
     * @param body
     *      master body
     * @param roles
     *      OCDS party roles
     * @return OCDS Organization
     */
    private static OCDSOrganization getOCDSOrganizationFromMasterBody(final MasterBody body,
        final OCDSPartyRole... roles) {

        if (body == null) {
            return null;
        }
        
        OCDSOrganization ocdsOrganization = new OCDSOrganization()
            .setId(body.getId())
            .setName(body.getName())
            .setAddress(getOCDSAddressFromAddress(body.getAddress()))
            .setRoles(new ArrayList<>(Arrays.asList(roles)));

        // body identifiers except ETALON_ID and BVD_ID
        if (body.getBodyIds() != null) {
            body.getBodyIds().stream()
                .filter(n -> !Arrays.asList(BodyIdentifier.Type.ETALON_ID, BodyIdentifier.Type.BVD_ID)
                    .contains(n.getType()))
                .forEach(n -> {
                    if (n.getType().equals(BodyIdentifier.Type.VAT)) {
                        ocdsOrganization.setIdentifier(getOCDSIdentifierFromBodyIdentifier(n));
                    } else {
                        ocdsOrganization.addAdditionalIdentifier(getOCDSIdentifierFromBodyIdentifier(n));
                    }
                });

            if (ocdsOrganization.getAdditionalIdentifiers() != null) {
                ocdsOrganization.setAdditionalIdentifiers(ocdsOrganization.getAdditionalIdentifiers().stream()
                    .filter(distinct(n -> n.getId()))
                    .collect(Collectors.toList()));
            }
        }

        ocdsOrganization.setContactPoint(getOCDSContactPoint(body));

        return ocdsOrganization;
    }

    /**
     * @param body
     *      master body to be parse from
     * @return OCDS contact point in case that at least one parameter is not null, otherwise null
     */
    private static OCDSContactPoint getOCDSContactPoint(final MasterBody body) {
        String name = getFirstNotNull(body.getContactName(), body.getContactPoint());
        String email = body.getEmail();
        String phone = body.getPhone();
        URL url = body.getAddress() != null ? body.getAddress().getUrl() : null;

        if (name == null && email == null && phone == null && url == null) {
            return null;
        }
        
        return new OCDSContactPoint().setName(name).setEmail(email).setPhone(phone).setUrl(url);
    }

    /**
     * Gets OCDS Organization with BUYER role from MasterBody.
     *
     * @param body
     *      master body
     * @return OCDS Organization
     */
    private static OCDSOrganization getOCDSBuyerFromMasterBody(final MasterBody body) {
        return getOCDSOrganizationFromMasterBody(body, OCDSPartyRole.BUYER);
    }
    
    /**
     * Gets OCDS Identifier from BOdyIdentifier.
     *
     * @param identifier
     *      body identifier
     * @return OCDS Identifier
     */
    private static OCDSIdentifier getOCDSIdentifierFromBodyIdentifier(final BodyIdentifier identifier) {
        if (identifier == null) {
            return null;
        }
        
        return new OCDSIdentifier().setId(identifier.getId()).setScheme(identifier.getType().name());
    }

    /**
     * Gets OCDS Address from Address.
     * 
     * @param address
     *      address
     * @return OCDS Address
     */
    private static OCDSAddress getOCDSAddressFromAddress(final Address address) {
        if (address == null) {
            return null;
        }
        
        return new OCDSAddress()
            .setStreet(address.getStreet())
            .setRegion(address.getState())
            .setPostcode(address.getPostcode())
            .setCountry(address.getCountry());        
    }

    /**
     * @param <U>
     *      value class
     * @param values
     *      values to be tested.
     * @return first not null value if exists or null
     */
    private static <U> U getFirstNotNull(final U... values) {
        return Arrays.asList(values).stream().filter(n -> n != null).findFirst().orElse(null);
    }

    /**
     * @param startDate
     *      start date
     * @param endDate
     *      end date
     * @param duration
     *      period duration in days
     * @return OCDS period only and only if at least one of passed parameters is not null
     */
    private static OCDSPeriod getOCDSPeriod(final LocalDate startDate, final LocalDate endDate,
        final Integer duration) {
        return getOCDSPeriod(getDateTime(startDate), getDateTime(endDate), duration);
    }

    /**
     * @see LocalDate#atStartOfDay()
     * @param date
     *      date
     * @return datetime
     */
    private static LocalDateTime getDateTime(final LocalDate date) {
        return date == null ? null : date.atStartOfDay();
    }

    /**
     * @param startDateTime
     *      start date time
     * @param endDateTime
     *      end date time
     * @param duration
     *      period duration in days
     * @return OCDS period only and only if at least one of passed parameters is not null
     */
    private static OCDSPeriod getOCDSPeriod(final LocalDateTime startDateTime, final LocalDateTime endDateTime,
        final Integer duration) {
        if (startDateTime != null || endDateTime != null || duration != null) {
            return new OCDSPeriod().setStartDate(startDateTime).setEndDate(endDateTime).setDurationInDays(duration);
        }

        return null;
    }

    /**
     * @param startDateTime
     *      start date time
     * @param endDateTime
     *      end date time
     * @return OCDS period only and only if at least one of passed parameters is not null
     */
    private static OCDSPeriod getOCDSPeriod(final LocalDateTime startDateTime, final LocalDateTime endDateTime) {
        return getOCDSPeriod(startDateTime, endDateTime, null);
    }

    /**
     * @param startDate
     *      start date
     * @param endDate
     *      end date
     * @return OCDS period only and only if at least one of passed parameters is not null
     */
    private static OCDSPeriod getOCDSPeriod(final LocalDate startDate, final LocalDate endDate) {
        return getOCDSPeriod(getDateTime(startDate), getDateTime(endDate), null);
    }

    /**
     * @param cpv
     *      cpv
     * @param relatedLot
     *      related lot id
     * @return OCDS CPV
     */
    private static OCDSItem getOCDSCPV(final CPV cpv, final String relatedLot) {
        if (cpv == null) {
            return null;
        }

        return new OCDSItem()
            .setId(Index.ITEM.next())
            .setRelatedLot(relatedLot)
            .setClassification(new OCDSItemClassification()
                .setScheme("CPV")
                .setId(cpv.getCode()));
    }

    /**
     * @param cpv
     *      cpv
     * @return OCDS CPV
     */
    private static OCDSItem getOCDSCPV(final CPV cpv) {
        return getOCDSCPV(cpv, null);
    }

    /**
     *
     * @param price
     *      price
     * @param fields
     *      price fields names to be tested
     * @return OCDSValue
     */
    private static OCDSValue getOCDSValueFromPrice(final Price price, final String... fields) {
        if (price == null) {
            return null;
        }

        for (String f : fields) {
            try {
                Object value = Price.class.getMethod("get" + WordUtils.capitalize(f)).invoke(price);

                if (value == null) {
                    continue;
                } else if (!(value instanceof BigDecimal)) {
                    logger.debug("Price field '{}' doesn't include BigDecimal", f);
                    continue;
                }

                return new OCDSValue().setAmount((BigDecimal) value).setCurrency(price.getCurrency());
            } catch (ReflectiveOperationException | SecurityException ex) {
                logger.debug("Unable to get value of the Price field '{}'", f);
            }
        }

        return null;
    }

    /**
     * @param document
     *      document
     * @return OCDSDocument
     */
    private static OCDSDocument getOCDSDocumentFromDocument(final Document document) {
        if (document == null) {
            return null;
        }

        return new OCDSDocument()
            .setId(Index.DOCUMENT.next())
            .setType(OCDSDocumentType.from(document.getType()))
            .setTitle(document.getTitle())
            .setDescription(document.getDescription())
            .setUrl(document.getUrl())
            .setPublished(getDateTime(document.getPublicationDate()))
            .setFormat(document.getFormat())
            .setLanguage(document.getLanguage());
    }

    /**
     * @param publication
     *      publication
     * @return OCDSDocument
     */
    private static OCDSDocument getOCDSDocumentFromPublication(final Publication publication) {
        if (publication == null) {
            return null;
        }

        return new OCDSDocument()
            .setId(Index.DOCUMENT.next())
            .setType(OCDSDocumentType.from(publication.getFormType()))
            .setUrl(ObjectUtils.firstNonNull(publication.getMachineReadableUrl(), publication.getHumanReadableUrl()))
            .setPublished(getDateTime(publication.getPublicationDate()))
            .setModified(getDateTime(publication.getLastUpdate()))
            .setLanguage(publication.getLanguage());
    }

     /**
     * Gets OCDS Organization reference from MasterBody with id and name set.
     *
     * @param body
     *      master body
     * @return OCDS Organization reference
     */
    private static OCDSOrganizationReference getOCDSOrganizationReferenceFromMasterBody(final MasterBody body) {
        if (body == null) {
            return null;
        }

        return new OCDSOrganizationReference().setId(body.getId()).setName(body.getName());
    }

    /**
     * @param value
     *      measured value
     * @param type
     *      statistic type
     * @param relatedLot
     *      related lot, can be null
     * @return OCDS bid statistic or null
     */
    private static OCDSBidStatistic getOCDSBidStatistic(final Integer value, final OCDSBidStatisticMeasure type,
        final String relatedLot) {

        if (value == null || type == null) {
            return null;
        }
        
        return new OCDSBidStatistic()
            .setId(Index.STATISTIC.next())
            .setMeasure(type)
            .setValue(value)
            .setRelatedLot(relatedLot);
    }

    /**
     * @param lot
     *      lot for witch the statistics are calculated
     * @return non-empty list of statistics or null
     */
    private static List<OCDSBidStatistic> getOCDSBidStatistics(final MasterTenderLot lot) {
        if (lot == null) {
            return null;
        }
        
        List<OCDSBidStatistic> stats = new ArrayList<>();

        String relatedLot = Index.LOT.current();

        stats.add(OCDSUtils.getOCDSBidStatistic(lot.getBidsCount(), OCDSBidStatisticMeasure.BIDS, relatedLot));
        stats.add(OCDSUtils.getOCDSBidStatistic(lot.getValidBidsCount(), OCDSBidStatisticMeasure.VALID_BIDS,
            relatedLot));
        stats.add(OCDSUtils.getOCDSBidStatistic(lot.getElectronicBidsCount(), OCDSBidStatisticMeasure.ELECTRONIC_BIDS,
            relatedLot));
        stats.add(OCDSUtils.getOCDSBidStatistic(lot.getSmeBidsCount(), OCDSBidStatisticMeasure.SME_BIDS,
            relatedLot));
        stats.add(OCDSUtils.getOCDSBidStatistic(lot.getForeignCompaniesBidsCount(),
            OCDSBidStatisticMeasure.FOREIGN_BIDS, relatedLot));
        stats.add(OCDSUtils.getOCDSBidStatistic(lot.getOtherEuMemberStatesCompaniesBidsCount(),
            OCDSBidStatisticMeasure.FOREIGN_BIDS_FROM_EU, relatedLot));

        stats = stats.stream().filter(Objects::nonNull).collect(Collectors.toList());
        
        return stats.isEmpty() ? null : stats;
    }

    /**
     * @param lot
     *      master tender lot
     * @return OCDS lot
     */
    private static OCDSLot getOCDSLotFromTenderLot(final MasterTenderLot lot) {
        if (lot == null) {
            return null;
        }

        return new OCDSLot()
            .setId(Index.LOT.current())
            .setTitle(lot.getTitle())
            .setDescription(lot.getDescription())
            .setValue(OCDSUtils.getOCDSValueFromPrice(lot.getEstimatedPrice(), "netAmount", "maxNetAmount"));
    }

    /**
     * Returns OCDS organization reference with id and name set.
     *
     * @param organization
     *      OCDS organization
     * @return OCDS organization reference
     */
    private static OCDSOrganizationReference getOCDSOrganizationReference(final OCDSOrganization organization) {
        if (organization == null) {
            return null;
        }
        
        return new OCDSOrganizationReference().setId(organization.getId()).setName(organization.getName());
    }

    /**
     * Returns OCDS release package for the given list of master tenders.
     *
     * @param tenders
     *      list of master tenders
     * @param uri
     *      OCDS package uri
     * @return OCDS release package
     */
    public static OCDSReleasePackage getOCDSReleasePackage(final List<MasterTender> tenders, final String uri) {
        OCDSReleasePackage ocdsPackage = new OCDSReleasePackage()
            .addExtension(OCDSLot.EXTENSION_URL)
            .addExtension(OCDSBid.EXTENSION_URL)
            .addExtension("https://raw.githubusercontent.com/open-contracting/ocds_requirements_extension/master/"
                + "extension.json")
            .setUri(uri)
            .setPublisher(new OCDSPublisher().setName(Config.getInstance().getParam("ocds.publisher")))
            .setPublished(LocalDateTime.now())
            .setVersion("1.1");

        if (tenders != null) {
            tenders.stream()
                .forEach(t -> {
                    OCDSRelease release = getOCDSRelease(t);
                    if (release != null) {
                        ocdsPackage.addRelease(release);
                    }
                });
        }

        return ocdsPackage;
    }

    /**
     * Returns OCDS release package for the given master tender.
     *
     * @param tender
     *      master tender
     * @param uri
     *      OCDS package uri
     * @return OCDS release package
     */
    public static OCDSReleasePackage getOCDSReleasePackage(final MasterTender tender, final String uri) {
        return getOCDSReleasePackage(Arrays.asList(tender), uri);
    }

    /**
     * @return OCDS prefix from configuration (ocds.prefix) or throws an exception and it logs an error if the prefix
     * isn't set
     */
    private static String getOCDSPrefix() {
        String ocdsPrefix = Config.getInstance().getParam("ocds.prefix");

        if (ocdsPrefix == null || ocdsPrefix.isEmpty()) {
            logger.error("OCDS prefix is required");
            throw new UnrecoverableException("OCDS prefix is required");
        }

        return ocdsPrefix;
    }

    /**
     * @param tender
     *      master tender
     * @return included publication or throws an exception and it logs an error if the given tender doesn't contains
     * included publication 
     */
    private static Publication getIncludedPublication(final MasterTender tender) {
        return tender.getPublications().stream().filter(n -> Objects.equals(n.getIsIncluded(), Boolean.TRUE))
            .findFirst().orElse(null);
    }

    /**
     * @param tender
     *      master tender
     * @return contract period
     */
    private static OCDSPeriod getContractPeriod(final MasterTender tender) {
        Integer duration = tender.getEstimatedDurationInDays();

        if (duration == null) {
            if (tender.getEstimatedDurationInMonths() != null) {
                duration = tender.getEstimatedDurationInMonths() * 30;
            } else if (tender.getEstimatedDurationInYears() != null) {
                duration = tender.getEstimatedDurationInYears() * 365;
            }
        }

        return OCDSUtils.getOCDSPeriod(tender.getEstimatedStartDate(), tender.getEstimatedCompletionDate(), duration);
    }

    /**
     * @param tender
     *      master tender
     * @param id
     *      id for OCDS tender
     * @return OCDS tender
     */
    private static OCDSTender getOCDSTender(final MasterTender tender, final String id) {
        List<OCDSDocument> docs = arrayWalk(tender.getPublications(), OCDSUtils::getOCDSDocumentFromPublication);

        OCDSTender ocdsTender = new OCDSTender()
            .setId(id)
            .setTitle(tender.getTitle())
            .setDescription(tender.getDescription())
            .setProcurementMethod(OCDSProcedureMethod.from(tender.getProcedureType()))
            .setProcurementMethodDetails(tender.getNationalProcedureType())
            .setProcurementMethodRationale(tender.getAcceleratedProcedureJustification())
            .setMainProcurementCategory(OCDSProcurementCategory.from(tender.getSupplyType()))
            .setAwardCriteria(OCDSAwardCriteria.from(tender.getSelectionMethod()))
            .setAwardCriteriaDetails(tender.getAwardCriteria() != null
                ? tender.getAwardCriteria().stream()
                    // award criterion to string
                    .map(n -> (n.getName() == null ? "" : n.getName()) + ": "
                        + (n.getWeight() == null ? "" : n.getWeight().toString()))
                    // criteria as combined string (criteria separated by new line)
                    .reduce((n, m) -> n + "\n" + m).get()
                : null)
            .setEligibilityCriteria(tender.getEligibilityCriteria())
            .setTenderPeriod(OCDSUtils.getOCDSPeriod(null, tender.getBidDeadline()))
            .setAwardPeriod(OCDSUtils.getOCDSPeriod(null, tender.getAwardDeadline()))
            .setEnquiryPeriod(OCDSUtils.getOCDSPeriod(null, tender.getEnquiryDeadline()))
            .setContractPeriod(getContractPeriod(tender))
            .setValue(OCDSUtils.getOCDSValueFromPrice(tender.getEstimatedPrice(), "netAmount", "maxNetAmount"))
            .setMinValue(OCDSUtils.getOCDSValueFromPrice(tender.getEstimatedPrice(), "minNetAmount"))
            .addDocuments(getOCDSTenderDocuments(docs));


        if (Objects.equals(tender.getIsElectronicAuction(), Boolean.TRUE)) {
            ocdsTender.addSubmissionMethod(OCDSSubmissionMethod.ELECTRONIC_AUCTION);
        }

        ocdsTender.setSubmissionMethodDetails(serializeBidsRecipient(tender.getBidsRecipient()));

        return ocdsTender;
    }

    /**
     * @param bidsRecipient
     *      bids recipient
     * @return serialized bids recipient or null
     */
    private static String serializeBidsRecipient(final MasterBody bidsRecipient) {
        if (bidsRecipient == null) {
            return null;
        }

        StringBuilder serialized = new StringBuilder();
        for (String p : new String[] {"bodyIds", "name", "email", "contactPoint", "contactName", "phone",
            "address.street", "address.city", "address.postcode", "address.nuts", "address.country", "address.state",
            "address.url"}) {
            
            Object value = ClassUtils.getProperty(bidsRecipient, p);
            if (value == null) {
                continue;
            }
            
            if (p.equals("bodyIds")) {
                value = String.join(",", ((List<BodyIdentifier>) value).stream()
                    .map(n -> n.getId()).collect(Collectors.toList()));
            } else if (p.equals("address.nuts")) {
                value = String.join(",", (List<String>) value);
            }

            if (!value.toString().isEmpty()) {
                serialized.append(p).append(": ").append(value).append("\\n");
            }
        }

        return serialized.length() > 0 ? serialized.toString() : null;
    }

    /**
     * Returns leader from bodies list if exists, otherwise first element. For empty list returns null.
     * @param bodies
     *      list of bodies
     * @return leader
     */
    private static MasterBody getLeaderBody(final List<MasterBody> bodies) {
        if (bodies == null || bodies.isEmpty()) {
            return null;
        }

        return bodies.stream().filter(n -> Objects.equals(n.getIsLeader(), Boolean.TRUE))
            .findFirst().orElse(bodies.get(0));
    } 

    /**
     * Returns OCDSRelease for the given master tender.
     *
     * @param tender
     *      master tender
     * @return OCDS release
     */
    public static OCDSRelease getOCDSRelease(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

        // BUYER
        OCDSOrganization ocdsBuyer = getOCDSReleaseBuyer(tender);
        if (ocdsBuyer == null){
            logger.warn("Tender {} has no buyers will be skipped.", tender.getId());
            return null;
        } else if (ocdsBuyer.getName() == null) {
            logger.warn("Buyer name is not set, tender {} will be skipped.", tender.getId());
            return null;
        }

        Index.resetAll();

        final String ocdsPrefix = getOCDSPrefix();

        final Publication includedPublication = getIncludedPublication(tender);

        final List<OCDSDocument> ocdsDocuments = arrayWalk(tender.getDocuments(),
            OCDSUtils::getOCDSDocumentFromDocument);

        final List<OCDSDocument> ocdsPublications = arrayWalk(tender.getPublications(),
            OCDSUtils::getOCDSDocumentFromPublication);

        List<OCDSItem> tenderCpvs = arrayWalk(tender.getCpvs(), OCDSUtils::getOCDSCPV);

        final boolean isOnBehalfOf = Objects.equals(tender.getIsOnBehalfOf(), Boolean.TRUE);

        // METADATA
        OCDSRelease ocds = new OCDSRelease()        
            .setOcid(ocdsPrefix + "-" + tender.getId())
            .setId(tender.getId())
            .setInitiationType(OCDSInitiationType.TENDER)
            .addTag(OCDSReleaseTag.COMPILED);

        String publicationSourceId = null;
        if (includedPublication != null) {
            publicationSourceId = includedPublication.getSourceId();
            
            ocds.setDate(includedPublication.getPublicationDate() != null
                    ? includedPublication.getPublicationDate().atStartOfDay() : null)
                .setLanguage(includedPublication.getLanguage());
        }

        // BUYER
        ocds.setBuyer(OCDSUtils.getOCDSOrganizationReference(ocdsBuyer));
        updateOCDSParties(ocds, tender.getBuyers().stream().map(
            n -> OCDSUtils.getOCDSOrganizationFromMasterBody(n, OCDSPartyRole.BUYER)).collect(Collectors.toList()));

        //TENDER
        OCDSTender ocdsTender = getOCDSTender(tender, getFirstNotNull(publicationSourceId, tender.getId()))
            .setProcuringEntity(isOnBehalfOf ? OCDSUtils.getOCDSOrganizationReference(ocdsBuyer) : null)
            .addItems(tenderCpvs)
            .addDocuments(getOCDSTenderDocuments(ocdsDocuments));
        
        if (tender.getLots() != null) {
            OCDSBid bids = new OCDSBid();

            for (MasterTenderLot l : tender.getLots()) {
                Index.LOT.next();

                List<OCDSItem> lotCpvs = arrayWalk(l.getCpvs(), c -> OCDSUtils.getOCDSCPV(c, Index.LOT.current()));

                ocdsTender.addItems(lotCpvs);
                
                bids.addStatistics(OCDSUtils.getOCDSBidStatistics(l));

                // AWARDS + BIDS + CONTRACTS
                if (l.getBids() != null) {                    
                    for (MasterBid b : l.getBids()) {
                        Index.BID.next();
                                                
                        // BID
                        final OCDSPartyRole tendererRole = Objects.equals(b.getIsWinning(), Boolean.TRUE)
                            ? OCDSPartyRole.SUPPLIER : OCDSPartyRole.TENDERER;
                        final List<OCDSOrganization> tenderers = arrayWalk(b.getBidders(), bidder -> {
                            return OCDSUtils.getOCDSOrganizationFromMasterBody(bidder, tendererRole);
                        });
                        updateOCDSParties(ocds, tenderers);

                        bids.addDetail(new OCDSBidDetail()
                            .setId(Index.BID.current())
                            .addRelatedLot(Index.LOT.current())
                            .setTenderers(arrayWalk(tenderers, OCDSUtils::getOCDSOrganizationReference))
                            .setValue(OCDSUtils.getOCDSValueFromPrice(b.getPrice(), "netAmount"))
                            .addDocuments(arrayWalk(b.getDocuments(), OCDSUtils::getOCDSDocumentFromDocument)));

                        // AWARD + CONTRACT
                        if (Objects.equals(b.getIsWinning(), Boolean.TRUE)) {
                            MasterBody supplier = OCDSUtils.getLeaderBody(b.getBidders());

                            // AWARD
                            OCDSAward award = new OCDSAward()
                                .setId(Index.AWARD.next())
                                .setRelatedBid(Index.BID.current())
                                .addRelatedLot(Index.LOT.current())
                                .setDate(getDateTime(OCDSUtils.getFirstNotNull(l.getAwardDecisionDate(),
                                    tender.getAwardDecisionDate())))
                                .setValue(OCDSUtils.getOCDSValueFromPrice(OCDSUtils.getFirstNotNull(b.getPrice(),
                                    tender.getFinalPrice()), "netAmount"))
                                .addDocuments(getOCDSAwardDocuments(ocdsPublications))
                                .addItems(lotCpvs)
                                .addSuppliers(arrayWalk(tenderers, OCDSUtils::getOCDSOrganizationReference));

                            ocds.addAward(award);

                            // CONTRACT
                            List<OCDSTransaction> transactions = arrayWalk(b.getPayments(), p -> new OCDSTransaction()
                                .setId(Index.TRANSACTION.next())
                                .setDate(getDateTime(p.getPaymentDate()))
                                .setValue(OCDSUtils.getOCDSValueFromPrice(p.getPrice(), "netAmount"))
                                .setPayer(OCDSUtils.getOCDSOrganizationReference(ocdsBuyer))
                                .setPayee(tenderers == null ? null : OCDSUtils.getOCDSOrganizationReference(
                                    OCDSUtils.getOCDSOrganizationFromMasterBody(supplier,
                                        OCDSPartyRole.SUPPLIER))));

                            LocalDateTime contractSigned = getDateTime(tender.getContractSignatureDate());
                            List<OCDSDocument> contractDocuments = getOCDSContractDocuments(ocdsDocuments);

                            if (contractSigned != null || contractDocuments != null || transactions != null) {
                                ocds.addContract(new OCDSContract()
                                    .setId(Index.CONTRACT.next())
                                    .setAwardId(Index.AWARD.current())
                                    .setSigned(getDateTime(tender.getContractSignatureDate()))
                                    .addDocuments(contractDocuments)
                                    .setImplementation(new OCDSImplementation().setTransactions(transactions)));
                            }
                        }               
                    }
                }

                ocdsTender.addLot(OCDSUtils.getOCDSLotFromTenderLot(l));
            }
            
            ocds.setBids(bids);
        }

        ocds.setTender(ocdsTender);

        return ocds;
    }

    /**
     * @param documents
     *      list of OCDS documents
     * @param type
     *      one or more documents types to be filtered
     * @return non-empty list of documents of given types or null
     */
    private static List<OCDSDocument> getOCDSDocumentsByType(final List<OCDSDocument> documents,
        final OCDSDocumentType... type) {
        if (documents == null) {
            return null;
        }

        List<OCDSDocument> ocdsDocuments = documents.stream().filter(n -> Arrays.asList(type).contains(n.getType()))
            .collect(Collectors.toList());
        
        return ocdsDocuments.isEmpty() ? null : ocdsDocuments;
    }

    /**
     * @param documents
     *      OCDS documents
     * @return non-empty list of documents published for OCDSTender or null
     */
    private static List<OCDSDocument> getOCDSTenderDocuments(final List<OCDSDocument> documents) {
        return getOCDSDocumentsByType(documents, OCDSDocumentType.TENDER_NOTICE, OCDSDocumentType.BIDDING_DOCUMENTS,
            OCDSDocumentType.ELIGIBILITY_CRITERIA, OCDSDocumentType.CLARIFICATIONS, OCDSDocumentType.SHORTLISTED_FIRMS,
            OCDSDocumentType.CONTRACT_DRAFT, OCDSDocumentType.CANCELLATION_DETAILS, OCDSDocumentType.COMPLAINTS);
    }

    /**
     * @param documents
     *      OCDS documents
     * @return non-empty list of documents published for OCDSAward or null
     */
    private static List<OCDSDocument> getOCDSAwardDocuments(final List<OCDSDocument> documents) {
        return getOCDSDocumentsByType(documents, OCDSDocumentType.AWARD_NOTICE, OCDSDocumentType.EVALUATION_REPORTS,
            OCDSDocumentType.COMPLAINTS, OCDSDocumentType.CONTRACT_DRAFT);
    }

    /**
     * @param documents
     *      OCDS documents
     * @return non-empty list of documents published for OCDSContract or null
     */
    private static List<OCDSDocument> getOCDSContractDocuments(final List<OCDSDocument> documents) {
        return getOCDSDocumentsByType(documents, OCDSDocumentType.CONTRACT_ANNEXE,
            OCDSDocumentType.CANCELLATION_DETAILS);
    }

    /**
     * Parses OCDS buyer from the given tender.
     *
     * @param tender
     *      master tender
     * @return OCDS buyer or null
     */
    private static OCDSOrganization getOCDSReleaseBuyer(final MasterTender tender) {
        MasterBody buyer = OCDSUtils.getLeaderBody(tender.getBuyers());

        OCDSOrganization ocdsBuyer = OCDSUtils.getOCDSOrganizationFromMasterBody(buyer, OCDSPartyRole.BUYER);

        if (ocdsBuyer != null) {
            if (Objects.equals(tender.getIsOnBehalfOf(), Boolean.TRUE)) {
                MasterBody behalfOf = OCDSUtils.getLeaderBody(tender.getOnBehalfOf());
                ocdsBuyer.setName(behalfOf != null ? behalfOf.getName() : null);
            } else {
                ocdsBuyer.setName(buyer.getName());
            }
        }

        return ocdsBuyer;
    }

    /**
     * For each not null element from the {@code list} calls the given {@code function}. Each not null result of
     * the call is added to the list of output entities.
     *
     * @param <T>
     *      input entity class
     * @param <U>
     *      output entity class
     * @param list
     *      list of input entities
     * @param function
     *      lambda function that transforms the T entity to the U entity.
     * @return non-empty list of U entities, otherwise null
     */
    private static <T, U> List<U> arrayWalk(final List<T> list, final Function<T, U> function) {
        if (list == null || list.isEmpty()) {
            return null;
        }

        final List<U> result = list.stream()
            .filter(Objects::nonNull)                           // non-null T
            .map(i -> function.apply(i))                        // transform T to U
            .filter(Objects::nonNull)                           // non-null U
            .collect(Collectors.toList());

        return (!result.isEmpty() ? result : null);
    }

    /**
     * Distinct keys in stream.
     *
     * @param keyExtractor
     *      key extractor
     * @param <T>
     *      list item class
     * @return distinct result
     */
    private static <T> Predicate<T> distinct(final Function<? super T, ?> keyExtractor) {
        Map<Object, Boolean> seen = new ConcurrentHashMap<>();
        return t -> seen.putIfAbsent(keyExtractor.apply(t), Boolean.TRUE) == null;
    }

    /**
     * Updates list of parties for the given OCDS release.
     * 
     * @param release
     *      OCDS release
     * @param parties
     *      OCDS organizations (parties) to be added/updated
     */
    private static void updateOCDSParties(final OCDSRelease release, final List<OCDSOrganization> parties) {
        if (release == null || parties == null) {
            return;
        }
        
        if (release.getParties() == null) {
            release.addParties(parties);
        } else {
            parties.forEach(p -> { updateOCDSParties(release, p); });
        }
    }

    /**
     * Adds new party (OCDSRelease.parties doesn't include this party) or update set of roles of existing party
     * (OCDSRelease.parties includes this party).
     *
     * @param release
     *      OCDS release into witch will be added party
     * @param party
     *      OCDS organization to be added
     */
    private static void updateOCDSParties(final OCDSRelease release, final OCDSOrganization party) {
        if (release == null || party == null) {
            return;
        }

        // check whether party exists in release parties list
        if (release.getParties() == null || release.getParties().stream()
            .noneMatch(n -> Objects.equals(n.getId(), party.getId()))) {
            release.addParty(party);
        } else {
            OCDSOrganization org = release.getParties().stream().filter(n -> Objects.equals(n.getId(), party.getId()))
                .findFirst().get();

            // update list of roles for existing party
            party.getRoles().stream()
                .filter(r -> { return org.getRoles().stream().noneMatch(n -> n.equals(r)); })
                .forEach(r -> org.addRole(r));
        }
    }
}
