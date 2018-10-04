package eu.dl.dataaccess.dto.matched;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.CleanBody;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BodyType;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.utils.DigestUtils;
import eu.dl.dataaccess.utils.WeightedHash;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Body (company, organization, ...).
 */
@Transformable
public class MatchedBody extends BaseMatchedStorableDTO
    implements Matchable, ManuallyMatchable, ExactellyMatchable, ApproximatellyMatchable, MasterablePart {

    /**
     * List of different body identification numbers (VAT or other unique
     * country ID).
     */
    private List<BodyIdentifier> bodyIds;

    /**
     * Official body name.
     */
    private String name;

    /**
     * Address of the body seat.
     */
    private Address address;

    /**
     * E-mail address.
     */
    private String email;

    /**
     * Description of contact point.
     */
    private String contactPoint;

    /**
     * Name of the contact person.
     */
    private String contactName;

    /**
     * Phone number of the contact.
     */
    private String phone;

    /**
     * Identifies the leader of consortium (typically buyer or bidder).
     */
    private Boolean isLeader;

    /**
     * List of main activity of body (eg. national authority, regional
     * authority, public body, etc.)
     */
    private List<BuyerActivityType> mainActivities;

    /**
     * Type of buyer.
     */
    private BuyerType buyerType;

    /**
     * Public buyer.
     */
    private Boolean isPublic;

    /**
     * Subsidized buyer.
     */
    private Boolean isSubsidized;

    /**
     * Sectoral buyer - has different TED form.
     */
    private Boolean isSectoral;

    /**
     * Body is a small-medium enterprise (SME).
     */
    private Boolean isSme;

    private String hash;

    private String groupId;

    private String matchedBy;

    /**
     * This flag is true when matched body is created from some external
     * reliable source.
     */
    private Boolean isPreferred;

    private String standardizedName;
    
    private String standardizedAddress;
    
    private String digest;

    private Double completenessScore;

    /**
     * Date of publication (of given version).
     */
    private LocalDate publicationDate;

    private BodyType role;

    private String source;

    private String fullHash;
    
    private List<WeightedHash> alternativeHashes;

    /**
     * Creates empty matched body.
     */
    public MatchedBody() {
        super();
    }

    /**
     * Creates matched body from clean body.
     * 
     * @param cleanBody
     *            instance of clean body
     * @param role
     *            the role of the body on the tender
     */
    public MatchedBody(final CleanBody cleanBody, final BodyType role) {
        super();
        setBodyIds(cleanBody.getBodyIds());
        setName(cleanBody.getName());
        setAddress(cleanBody.getAddress());
        setEmail(cleanBody.getEmail());
        setContactPoint(cleanBody.getContactPoint());
        setContactName(cleanBody.getContactName());
        setPhone(cleanBody.getPhone());
        setIsLeader(cleanBody.getIsLeader());
        setMainActivities(cleanBody.getMainActivities());
        setBuyerType(cleanBody.getBuyerType());
        setIsPublic(cleanBody.getIsPublic());
        setIsSubsidized(cleanBody.getIsSubsidized());
        setIsSectoral(cleanBody.getIsSectoral());
        setIsSme(cleanBody.getIsSme());

        String stdName = DigestUtils.standardizeName(cleanBody.getName());
        String stdAddress = DigestUtils.standardizeAddress(cleanBody.getAddress());

        setStandardizedName(stdName);
        setStandardizedAddress(stdAddress);
        setDigest(DigestUtils.digest(this));

        setRole(role);
    }

    /**
     * Gets the body ids.
     *
     * @return the bodyIds
     */
    @Override
    
    public final List<BodyIdentifier> getBodyIds() {
        return bodyIds;
    }

    /**
     * Sets the body ids.
     *
     * @param newBodyIds
     *            the bodyIds to set
     * @return this instance for chaining
     */
    public final MatchedBody setBodyIds(final List<BodyIdentifier> newBodyIds) {
        this.bodyIds = newBodyIds;
        return this;
    }

    /**
     * Adds an body identifier to the bodyIds list.
     *
     * @param bodyId
     *            body identifier
     * @return this instance for chaining
     */
    public final MatchedBody addBodyId(final BodyIdentifier bodyId) {
        if (bodyId != null) {
            if (this.bodyIds == null) {
                this.bodyIds = new ArrayList<>();
            }

            this.bodyIds.add(bodyId);
        }

        return this;
    }

    /**
     * Gets the name.
     *
     * @return the name
     */
    
    public final String getName() {
        return name;
    }

    /**
     * Sets the name.
     *
     * @param newName
     *            the name to set
     * @return this instance for chaining
     */
    public final MatchedBody setName(final String newName) {
        this.name = newName;
        return this;
    }

    /**
     * Gets the address.
     *
     * @return the address
     */
    
    public final Address getAddress() {
        return address;
    }

    /**
     * Sets the address.
     *
     * @param newAddress
     *            the address to set
     * @return this instance for chaining
     */
    public final MatchedBody setAddress(final Address newAddress) {
        this.address = newAddress;
        return this;
    }

    /**
     * Gets the email.
     *
     * @return the email
     */
    
    public final String getEmail() {
        return email;
    }

    /**
     * Sets the email.
     *
     * @param newEmail
     *            the email to set
     * @return this instance for chaining
     */
    public final MatchedBody setEmail(final String newEmail) {
        this.email = newEmail;
        return this;
    }

    /**
     * Gets the contact point.
     *
     * @return the contactPoint
     */
    
    public final String getContactPoint() {
        return contactPoint;
    }

    /**
     * Sets the contact point.
     *
     * @param newContactPoint
     *            the contactPoint to set
     * @return this instance for chaining
     */
    public final MatchedBody setContactPoint(final String newContactPoint) {
        this.contactPoint = newContactPoint;
        return this;
    }

    /**
     * Gets the contact name.
     *
     * @return the contactName
     */
    
    public final String getContactName() {
        return contactName;
    }

    /**
     * Sets the contact name.
     *
     * @param newContactName
     *            the contactName to set
     * @return this instance for chaining
     */
    public final MatchedBody setContactName(final String newContactName) {
        this.contactName = newContactName;
        return this;
    }

    /**
     * Gets the phone.
     *
     * @return the phone
     */
    
    public final String getPhone() {
        return phone;
    }

    /**
     * Sets the phone.
     *
     * @param newPhone
     *            the phone to set
     * @return this instance for chaining
     */
    public final MatchedBody setPhone(final String newPhone) {
        this.phone = newPhone;
        return this;
    }

    /**
     * Gets the checks if is leader.
     *
     * @return the isLeader
     */
    
    public final Boolean getIsLeader() {
        return isLeader;
    }

    /**
     * Sets the is leader.
     *
     * @param newIsLeader
     *            the isLeader to set
     * @return this instance for chaining
     */
    public final MatchedBody setIsLeader(final Boolean newIsLeader) {
        this.isLeader = newIsLeader;
        return this;
    }

    /**
     * Gets the main activities.
     *
     * @return the mainActivities
     */
    
    public final List<BuyerActivityType> getMainActivities() {
        return mainActivities;
    }

    /**
     * Sets the main activities.
     *
     * @param newMainActivities
     *            the mainActivities list to set
     * @return this instance for chaining
     */
    public final MatchedBody setMainActivities(final List<BuyerActivityType> newMainActivities) {
        this.mainActivities = newMainActivities;
        return this;
    }

    /**
     * Adds the main activity.
     *
     * @param mainActivity
     *            the mainActivities list to set
     * @return this instance for chaining
     */
    public final MatchedBody addMainActivity(final BuyerActivityType mainActivity) {
        if (mainActivity != null) {
            if (getMainActivities() == null) {
                setMainActivities(new ArrayList<>());
            }

            this.mainActivities.add(mainActivity);
        }

        return this;
    }

    /**
     * Gets the buyer type.
     *
     * @return the buyerType
     */
    
    public final BuyerType getBuyerType() {
        return buyerType;
    }

    /**
     * Sets the buyer type.
     *
     * @param newBuyerType
     *            the buyerType to set
     * @return this instance for chaining
     */
    public final MatchedBody setBuyerType(final BuyerType newBuyerType) {
        this.buyerType = newBuyerType;
        return this;
    }

    /**
     * Gets the checks if is public.
     *
     * @return the isPublic
     */
    
    public final Boolean getIsPublic() {
        return isPublic;
    }

    /**
     * Sets the is public.
     *
     * @param newIsPublic
     *            the isPublic to set
     * @return this instance for chaining
     */
    public final MatchedBody setIsPublic(final Boolean newIsPublic) {
        this.isPublic = newIsPublic;
        return this;
    }

    /**
     * Gets the checks if is subsidized.
     *
     * @return the isSubsidized
     */
    
    public final Boolean getIsSubsidized() {
        return isSubsidized;
    }

    /**
     * Sets the is subsidized.
     *
     * @param newIsSubsidized
     *            the isSubsidized to set
     * @return this instance for chaining
     */
    public final MatchedBody setIsSubsidized(final Boolean newIsSubsidized) {
        this.isSubsidized = newIsSubsidized;
        return this;
    }

    /**
     * Gets the checks if is sectoral.
     *
     * @return the isSectoral
     */
    
    public final Boolean getIsSectoral() {
        return isSectoral;
    }

    /**
     * Sets the is sectoral.
     *
     * @param newIsSectoral
     *            the isSectoral to set
     * @return this instance for chaining
     */
    public final MatchedBody setIsSectoral(final Boolean newIsSectoral) {
        this.isSectoral = newIsSectoral;
        return this;
    }

    /**
     * Gets the checks if is sme.
     *
     * @return the isSme
     */
    
    public final Boolean getIsSme() {
        return isSme;
    }

    /**
     * Sets the is sme.
     *
     * @param newIsSme
     *            the isSme to set
     * @return this instance for chaining
     */
    public final MatchedBody setIsSme(final Boolean newIsSme) {
        this.isSme = newIsSme;
        return this;
    }

    /**
     * Gets the group id.
     *
     * @return the group id
     */
    @Override
    public final String getGroupId() {
        return groupId;

    }

    /**
     * Sets the group id.
     *
     * @param newGroupId
     *            the new group id
     * @return the matched body
     */
    @Override
    public final MatchedBody setGroupId(final String newGroupId) {
        this.groupId = newGroupId;
        return this;
    }

    /**
     * Gets the matched by.
     *
     * @return the matched by
     */
    public final String getMatchedBy() {
        return matchedBy;
    }

    /**
     * Sets the matched by.
     *
     * @param newMatchedBy
     *            the new matched by
     * @return the matched body
     */
    public final MatchedBody setMatchedBy(final String newMatchedBy) {
        this.matchedBy = newMatchedBy;
        return this;
    }

    /**
     * Gets the hash.
     *
     * @return the hash
     */
    public final String getHash() {
        return hash;
    }

    /**
     * Sets the hash.
     *
     * @param newHash
     *            the new hash
     * @return the matched body
     */
    public final MatchedBody setHash(final String newHash) {
        this.hash = newHash;
        return this;
    }

    /**
     * Gets the checks if is preferred.
     *
     * @return the checks if is preferred
     */
    
    public final Boolean getIsPreferred() {
        return isPreferred;
    }

    /**
     * Sets the is preferred.
     *
     * @param newIsPreferred
     *            the new is preferred
     * @return the matched body
     */
    public final MatchedBody setIsPreferred(final Boolean newIsPreferred) {
        this.isPreferred = newIsPreferred;
        return this;
    }

    /**
     * @return standardized name
     */
    @Override
    public final String getStandardizedName() {
        return standardizedName;
    }

    /**
     * @param standardizedName 
     *      standardized name to set
     * @return this instance for chaining
     */
    public final MatchedBody setStandardizedName(final String standardizedName) {
        this.standardizedName = standardizedName;
        return this;
    }

    /**
     * @return standardized address
     */
    public final String getStandardizedAddress() {
        return standardizedAddress;
    }

    /**
     * @param standardizedAddress
     *      standardized address to set
     * @return this instance for chaining
     */
    public final MatchedBody setStandardizedAddress(final String standardizedAddress) {
        this.standardizedAddress = standardizedAddress;
        return this;
    }
    
    /**
     * Returns postcode of the body address.
     * 
     * @see #getAddress() 
     * 
     * @return postcode
     */
    @Override
    
    @JsonIgnore
    public final String getPostcode() {
        if (getAddress() == null) {
            return null;
        }
        
        return getAddress().getPostcode();
    }

    /**
     * Return list of NUTS codes of the body address.
     *
     * @see #getAddress() 
     * 
     * @return list of NUTS codes
     */
    @Override
    
    @JsonIgnore
    public final List<String> getNuts() {
        if (getAddress() == null) {
            return null;
        }
        
        return getAddress().getNuts();
    }

    @Override
    public final String getDigest() {
        return digest;
    }

    /**
     * Sets body digest.
     *
     * @param digest
     *      digest of the body to set
     * @return this instance for chaining
     */
    public final MatchedBody setDigest(final String digest) {
        this.digest = digest;
        return this;
    }

    /**
     * Return body score.
     * 
     * @return completeness score
     */
    public final Double getCompletenessScore() {
        return completenessScore;
    }

    /**
     * Sets body completeness score.
     *
     * @param completenessScore
     *            completeness score of this body
     * @return this instance for chaining
     */
    public final MatchedBody setCompletenessScore(final Double completenessScore) {
        this.completenessScore = completenessScore;
        return this;
    }

    @Override
    public final String getTenderId() {
        return null;
    }

    /**
     * @return the publicationDate
     */
    public final LocalDate getPublicationDate() {
        return publicationDate;
    }

    /**
     * @param publicationDate
     *            the publicationDate to set
     *
     * @return this instance for chaining
     */
    public final MatchedBody setPublicationDate(final LocalDate publicationDate) {
        this.publicationDate = publicationDate;
        return this;
    }

    /**
     * @return the role of body on tender
     */
    public final BodyType getRole() {
        return role;
    }

    /**
     * @param newRole
     *            role of body on tender
     *
     * @return this instance for chaining
     */
    public final MatchedBody setRole(final BodyType newRole) {
        this.role = newRole;
        return this;
    }

    /**
     * @return the source of body
     */
    public final String getSource() {
        return source;
    }

    /**
     * @param source
     *            source of body on tender
     *
     * @return this instance for chaining
     */
    public final MatchedBody setSource(final String source) {
        this.source = source;
        return this;
    }

    /**
     * @return full hash of body
     */
    @Override
    public final String getFullHash() {
        return fullHash;
    }

    /**
     * @param fullHash
     *      body full hash to be set
     * @return this instance for chaining
     */
    public final MatchedBody setFullHash(final String fullHash) {
        this.fullHash = fullHash;
        return this;
    }

    /**
     * Getter.
     * @return alternative hashes
     */
	public final List<WeightedHash> getAlternativeHashes() {
		return alternativeHashes;
	}

	/**
	 * Alternative hashes.
	 * @param alternativeHashes alternative body hashes
	 */
	public final void setAlternativeHashes(final List<WeightedHash> alternativeHashes) {
		this.alternativeHashes = alternativeHashes;
	}

    @Override
    public final LocalDateTime getCreatedRaw() {
        return null;
    }
}
