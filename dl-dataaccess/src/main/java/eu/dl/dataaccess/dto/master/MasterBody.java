package eu.dl.dataaccess.dto.master;

import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.matched.MatchedBody;

import java.util.ArrayList;
import java.util.List;

/**
 * Body (company, organization, ...).
 */
@Transformable
public class MasterBody extends BaseMasterStorableDTO implements Masterable {

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

    private String masterBy;

    /**
     * This flag is true when master body is created from some external reliable
     * source.
     */
    private Boolean isPreferred;

    /*
     * Indicators related to this tender.
     */
    private List<Indicator> indicators;
    
    /**
     * Creates empty master body.
     */
    public MasterBody() {
        super();
    }

    /**
     * Creates master body from matched body.
     * 
     * @param matchedBody
     *            instacne of matched body
     */
    public MasterBody(final MatchedBody matchedBody) {
        super();
        setBodyIds(matchedBody.getBodyIds());
        setName(matchedBody.getName());
        setAddress(matchedBody.getAddress());
        setEmail(matchedBody.getEmail());
        setContactPoint(matchedBody.getContactPoint());
        setContactName(matchedBody.getContactName());
        setPhone(matchedBody.getPhone());
        setIsLeader(matchedBody.getIsLeader());
        setMainActivities(matchedBody.getMainActivities());
        setBuyerType(matchedBody.getBuyerType());
        setIsPublic(matchedBody.getIsPublic());
        setIsSubsidized(matchedBody.getIsSubsidized());
        setIsSectoral(matchedBody.getIsSectoral());
        setIsSme(matchedBody.getIsSme());
        setGroupId(matchedBody.getGroupId());
    }

    /**
     * Gets the body ids.
     *
     * @return the bodyIds
     */
    
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
    public final MasterBody setBodyIds(final List<BodyIdentifier> newBodyIds) {
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
    public final MasterBody addBodyId(final BodyIdentifier bodyId) {
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
    public final MasterBody setName(final String newName) {
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
    public final MasterBody setAddress(final Address newAddress) {
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
    public final MasterBody setEmail(final String newEmail) {
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
    public final MasterBody setContactPoint(final String newContactPoint) {
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
    public final MasterBody setContactName(final String newContactName) {
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
    public final MasterBody setPhone(final String newPhone) {
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
    public final MasterBody setIsLeader(final Boolean newIsLeader) {
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
    public final MasterBody setMainActivities(final List<BuyerActivityType> newMainActivities) {
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
    public final MasterBody addMainActivity(final BuyerActivityType mainActivity) {
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
    public final MasterBody setBuyerType(final BuyerType newBuyerType) {
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
    public final MasterBody setIsPublic(final Boolean newIsPublic) {
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
    public final MasterBody setIsSubsidized(final Boolean newIsSubsidized) {
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
    public final MasterBody setIsSectoral(final Boolean newIsSectoral) {
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
    public final MasterBody setIsSme(final Boolean newIsSme) {
        this.isSme = newIsSme;
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see eu.dl.dataaccess.dto.master.Masterable#getGroupId()
     */
    @Override
    public final String getGroupId() {
        return groupId;

    }


    /*
     * (non-Javadoc)
     * 
     * @see eu.dl.dataaccess.dto.master.Masterable#setGroupId(java.lang.String)
     */
    @Override
    public final MasterBody setGroupId(final String newGroupId) {
        this.groupId = newGroupId;
        return this;
    }

    /**
     * Gets the master by.
     *
     * @return the master by
     */
    public final String getMasterBy() {
        return masterBy;
    }

    /**
     * Sets the master by.
     *
     * @param newMasterBy
     *            the new master by
     * @return the master body
     */
    public final MasterBody setMasterBy(final String newMasterBy) {
        this.masterBy = newMasterBy;
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
     * @return the master body
     */
    public final MasterBody setHash(final String newHash) {
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
     * @return the master body
     */
    public final MasterBody setIsPreferred(final Boolean newIsPreferred) {
        this.isPreferred = newIsPreferred;
        return this;
    }
    
    /**
     * @return the indicators
     */
    public final List<Indicator> getIndicators() {
        return indicators;
    }

    /**
     * @param indicators the indicators to set
     * 
     * @return the master tender
     */
    public final MasterBody setIndicators(final List<Indicator> indicators) {
        this.indicators = indicators;
        return this;
    }
}
