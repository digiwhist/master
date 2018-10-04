package eu.dl.dataaccess.dto.ocds;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.OCDSInitiationType;
import eu.dl.dataaccess.dto.codetables.OCDSReleaseTag;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * OCDS release. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSRelease {
    private String ocid;

    private String id;

    private LocalDateTime date;

    private List<OCDSReleaseTag> tags;

    private OCDSInitiationType initiationType;

    private String language;

    private OCDSOrganizationReference buyer;

    private List<OCDSOrganization> parties;

    private OCDSTender tender;
    
    private List<OCDSAward> awards;
    
    private List<OCDSContract> contracts;

    private OCDSBid bids;

    private OCDSPlanning planning;

    /**
     * @return OCDS id
     */
    public final String getOcid() {
        return ocid;
    }

    /**
     * @param ocid
     *      OCDS id to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setOcid(final String ocid) {
        this.ocid = ocid;
        return this;
    }

    /**
     * @return id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *      id to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return date
     */
    @JsonSerialize(using = OCDSLocalDateTimeSerializer.class)
    public final LocalDateTime getDate() {
        return date;
    }

    /**
     * @param date
     *      date to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setDate(final LocalDateTime date) {
        this.date = date;
        return this;
    }

    /**
     * @return list of tags
     */
    @JsonProperty("tag")
    public final List<OCDSReleaseTag> getTags() {
        return tags;
    }

    /**
     * @param tags
     *      list of tags to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setTags(final List<OCDSReleaseTag> tags) {
        this.tags = tags;
        return this;
    }

    /**
     * Adds tag. List is created if needed.
     *
     * @param tag
     *      tag to be added
     * @return this instance for chaining
     */
    public final OCDSRelease addTag(final OCDSReleaseTag tag) {
        if (tag != null) {
            if (this.tags == null) {
                this.tags = new ArrayList<>();
            }

            this.tags.add(tag);
        }

        return this;
    }

    /**
     * @return initiation type
     */
    public final OCDSInitiationType getInitiationType() {
        return initiationType;
    }

    /**
     * @param initiationType
     *      initiation type to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setInitiationType(final OCDSInitiationType initiationType) {
        this.initiationType = initiationType;
        return this;
    }

    /**
     * @return language
     */
    @JsonSerialize(using = OCDSLanguageSerializer.class)
    public final String getLanguage() {
        return language;
    }

    /**
     * @param language
     *      language to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setLanguage(final String language) {
        this.language = language;
        return this;
    }

    /**
     * @return buyer
     */
    public final OCDSOrganizationReference getBuyer() {
        return buyer;
    }

    /**
     * @param buyer
     *      buyer to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setBuyer(final OCDSOrganizationReference buyer) {
        this.buyer = buyer;
        return this;
    }

    /**
     * @return list of parties
     */
    public final List<OCDSOrganization> getParties() {
        return parties;
    }

    /**
     * @param parties
     *      list of parties to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setParties(final List<OCDSOrganization> parties) {
        this.parties = parties;
        return this;
    }

    /**
     * Adds party. List is created if needed.
     *
     * @param party
     *      party to be added
     * @return this instance for chaining
     */
    public final OCDSRelease addParty(final OCDSOrganization party) {
        if (party != null) {
            if (this.parties == null) {
                this.parties = new ArrayList<>();
            }

            this.parties.add(party);
        }

        return this;
    }

    /**
     * @return tender
     */
    public final OCDSTender getTender() {
        return tender;
    }

    /**
     * @param tender
     *      tender to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setTender(final OCDSTender tender) {
        this.tender = tender;
        return this;
    }

    /**
     * @return list of awards
     */
    public final List<OCDSAward> getAwards() {
        return awards;
    }

    /**
     * @param awards
     *      list of awards to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setAwards(final List<OCDSAward> awards) {
        this.awards = awards;
        return this;
    }

    /**
     * Adds award. List is created if needed.
     *
     * @param award
     *      award to be added
     * @return this instance for chaining
     */
    public final OCDSRelease addAward(final OCDSAward award) {
        if (award != null) {
            if (this.awards == null) {
                this.awards = new ArrayList<>();
            }

            this.awards.add(award);
        }

        return this;
    }

    /**
     * @return list of contracts
     */
    public final List<OCDSContract> getContracts() {
        return contracts;
    }

    /**
     * @param contracts
     *      list of contracts to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setContracts(final List<OCDSContract> contracts) {
        this.contracts = contracts;
        return this;
    }

    /**
     * Adds contract. List is created if needed.
     *
     * @param contract
     *      bid to be added
     * @return this instance for chaining
     */
    public final OCDSRelease addContract(final OCDSContract contract) {
        if (contract != null) {
            if (this.contracts == null) {
                this.contracts = new ArrayList<>();
            }

            this.contracts.add(contract);
        }

        return this;
    }

    /**
     * @return bids
     */
    public final OCDSBid getBids() {
        return bids;
    }

    /**
     * @param bids
     *      bids to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setBids(final OCDSBid bids) {
        this.bids = bids;
        return this;
    }

    /**
     * Adds parties. List is created if needed.
     *
     * @param newParties
     *      list of parties to be added
     * @return this instance for chaining
     */
    public final OCDSRelease addParties(final List<OCDSOrganization> newParties) {
        if (newParties != null && !newParties.isEmpty()) {
            if (this.parties == null) {
                this.parties = new ArrayList<>();
            }

            this.parties.addAll(newParties);
        }

        return this;
    }

    /**
     * @return planning
     */
    public final OCDSPlanning getPlanning() {
        return planning;
    }

    /**
     * @param planning
     *      planning to be set
     * @return this instance for chaining
     */
    public final OCDSRelease setPlanning(final OCDSPlanning planning) {
        this.planning = planning;
        return this;
    }
}
