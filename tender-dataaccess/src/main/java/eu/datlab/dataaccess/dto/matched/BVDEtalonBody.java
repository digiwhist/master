package eu.datlab.dataaccess.dto.matched;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.matched.EtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.utils.DigestUtils;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import static eu.dl.dataaccess.utils.DigestUtils.generateAlternativeBodyHashes;
import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * Etalon body for Bureau van Dijk.
 *
 * @author Tomas Mrazek
 */
public class BVDEtalonBody implements EtalonBody {
    private String city;
    
    private String country;
    
    private String nuts3;
    
    private String street;
    
    private String europeanVatNumber;
    
    private String vatTaxNumber;
    
    private String tradeRegisterNumber;
    
    private String statisticalNumber;
    
    private String bvdId;
            
    private String countryIsoCode;
    
    private String id;
    
    private String standardizedName;
    
    private String standardizedAddress;
    
    private String name;
    
    private String postcode;
    
    private String digest;

    private String companyCategory;

    private String status;

    private String statusChange;

    private String nace;

    private String numberOfEmployees;

    private String foundationDate;

    /**
     * Digest where the limit for name digest length are 2 characters.
     */
    private String digest2;

    @Override
    public final MatchedBody getAsMatchedBody() {
        MatchedBody matchedBody = new MatchedBody();
        
        matchedBody.setBodyIds(getBodyIds());       
        matchedBody.setName(getName());
        matchedBody.setStandardizedName(getStandardizedName());
        matchedBody.setStandardizedAddress(getStandardizedAddress());
        matchedBody.setAddress(new Address()
            .setCity(getCity())
            .setCountry(getCountryIsoCode())
            .setNuts(getNuts())
            .setPostcode(getPostcode())
            .setStreet(getStreet()));
        matchedBody.setDigest(getDigest());
        matchedBody.setSource(ETALON_SOURCE_ID);

        HashMap<String, Object> meta = new HashMap<>();

        meta.put("companyCategory", getCompanyCategory());
        meta.put("status", getStatus());
        meta.put("statusChange", getStatusChange());
        meta.put("nace", getNace());
        meta.put("numberOfEmployees", getNumberOfEmployees());
        meta.put("foundationDate", getFoundationDate());

        matchedBody.setMetaData(meta);
        
        // data cleaninig/validating
        matchedBody.setBodyIds(ValidationUtils.getValid(matchedBody.getBodyIds()));

        Address addr = matchedBody.getAddress();
        String regex = "((.*[^\\p{Alnum}])|^)(?<nuts>[A-Z]{2}[0-9A-Z]{0,3})(([^\\p{Alnum}].*)|$)";
        if (addr != null && addr.getNuts() != null) {
            addr.setNuts(addr.getNuts().stream()
                .filter(n -> n != null && n.matches(regex))
                .map(n -> n.replaceAll(regex, "${nuts}"))
                .collect(Collectors.toList()));
            
            addr = addr.getValid();
        }
        matchedBody.setAddress(addr);

        matchedBody.setAlternativeHashes(generateAlternativeBodyHashes(matchedBody));

        matchedBody.setFullHash(DigestUtils.bodyFullHash(matchedBody));
        
        return matchedBody;
    }
    
    /**
     * @return entity id as String
     */
    @Override
    public final String getId() {
        return id;
    }
    
    /**
     * @param id
     *            id
     * @return this for fluent interface
     */
    public final BVDEtalonBody setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * Gets the body ids.
     *
     * @return the bodyIds
     */
    @Override
    public final List<BodyIdentifier> getBodyIds() {
        final List<BodyIdentifier> bodyIds = new ArrayList<>();
        
        bodyIds.add(new BodyIdentifier()
                .setId(getId())
                .setType(BodyIdentifier.Type.ETALON_ID)
                .setScope(BodyIdentifier.Scope.ETALON_ID));
        
        if (getBvdId() != null && !getBvdId().trim().isEmpty()) {
	        bodyIds.add(new BodyIdentifier()
                .setId(getBvdId())
                .setType(BodyIdentifier.Type.BVD_ID)
                .setScope(BodyIdentifier.Scope.ETALON_ID));
        }
        
        if (getEuropeanVatNumber() != null && !getEuropeanVatNumber().trim().isEmpty()) {
            bodyIds.add(new BodyIdentifier()
                .setId(getEuropeanVatNumber())
                .setScope(BodyIdentifier.Scope.EU)
                .setType(BodyIdentifier.Type.TAX_ID));
        }
        
        if (getVatTaxNumber()!= null && !getVatTaxNumber().trim().isEmpty()) {
            bodyIds.add(new BodyIdentifier()
            .setId(getVatTaxNumber())
            .setScope(BodyIdentifier.Scope.valueOf(getCountryIsoCode()))
            .setType(BodyIdentifier.Type.TAX_ID));
        }
        
        if (getTradeRegisterNumber() != null && !getTradeRegisterNumber().trim().isEmpty()) {
            bodyIds.add(new BodyIdentifier()
                .setId(getTradeRegisterNumber())
                .setScope(BodyIdentifier.Scope.valueOf(getCountryIsoCode()))
                .setType(BodyIdentifier.Type.TRADE_REGISTER));
        }
        
        if (getStatisticalNumber() != null && !getStatisticalNumber().trim().isEmpty()) {
            bodyIds.add(new BodyIdentifier()
                .setId(getStatisticalNumber())
                .setScope(BodyIdentifier.Scope.valueOf(getCountryIsoCode()))
                .setType(BodyIdentifier.Type.STATISTICAL));
        }
        
        return bodyIds.isEmpty() ? null : bodyIds;
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
    public final BVDEtalonBody setStandardizedName(final String standardizedName) {
        this.standardizedName = standardizedName;
        return this;
    }

    /**
     * @return standardized address
     */
    @Override
    public final String getStandardizedAddress() {
        return standardizedAddress;
    }

    /**
     * @param standardizedAddress
     *      standardized address to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setStandardizedAddress(final String standardizedAddress) {
        this.standardizedAddress = standardizedAddress;
        return this;
    }
    
    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param newName
     *            the name to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setName(final String newName) {
        this.name = newName;
        return this;
    }
    
    /**
     * @return body address postcode
     */
    @Override
    public final String getPostcode() {
        return postcode;
    }

    /**
     * @param postcode
     *      body address postcode to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setPostcode(final String postcode) {
        this.postcode = postcode;
        return this;
    }
    
    /**
     * @return body address nuts
     */
    
    @Override
    public final List<String> getNuts() {
        if (getNuts3() == null) {
            return null;
        }
        
        return new ArrayList<>(Arrays.asList(getNuts3()));
    }
    
    @Override
    public final String getDigest() {
        return digest;
    }
    
    /**
     * Sets digest.
     *
     * @param digest
     *      digest to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setDigest(final String digest) {
        this.digest = digest;
        return this;
    }
    
    /**
     * @return body address city
     */
    public final String getCity() {
        return city;
    }

    /**
     * @param city
     *      body address city to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setCity(final String city) {
        this.city = city;
        return this;
    }

    /**
     * @return body address coutry
     */
    public final String getCountry() {
        return country;
    }

    /**
     * @param country
     *      body address country to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setCountry(final String country) {
        this.country = country;
        return this;
    }

    /**
     * @return body address NUTS3 code
     */
    public final String getNuts3() {
        return nuts3;
    }

    /**
     * @param nuts3
     *      body address NUTS3 code to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setNuts3(final String nuts3) {
        this.nuts3 = nuts3;
        return this;
    }

    /**
     * @return body address street
     */
    public final String getStreet() {
        return street;
    }

    /**
     * @param street
     *      body address street to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setStreet(final String street) {
        this.street = street;
        return this;
    }

    /**
     * @return european vat number
     */
    public final String getEuropeanVatNumber() {
        return europeanVatNumber;
    }

    /**
     * @param europeanVatNumber 
     *      european vat number to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setEuropeanVatNumber(final String europeanVatNumber) {
        this.europeanVatNumber = europeanVatNumber;
        return this;
    }

    /**
     * @return vat tax number
     */
    public final String getVatTaxNumber() {
        return vatTaxNumber;
    }

    /**
     * @param newVatTaxNumber 
     *      vat tax number to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setVatTaxNumber(final String newVatTaxNumber) {
        this.vatTaxNumber = newVatTaxNumber;
        return this;
    }

    /**
     * @return trade register number
     */
    public final String getTradeRegisterNumber() {
        return tradeRegisterNumber;
    }

    /**
     * @param tradeRegisterNumber
     *      trade register number
     * @return this instance for chaining
     */    
    public final BVDEtalonBody setTradeRegisterNumber(final String tradeRegisterNumber) {
        this.tradeRegisterNumber = tradeRegisterNumber;
        return this;
    }

    /**
     * @return bvdId
     */
    public final String getBvdId() {
        return bvdId;
    }

    /**
     * @param bvdId 
     *      bvdId
     * @return this instance for chaining
     */
    public final BVDEtalonBody setBvdId(final String bvdId) {
        this.bvdId = bvdId;
        return this;
    }
    
    /**
     * @return statistical number
     */
    public final String getStatisticalNumber() {
        return statisticalNumber;
    }

    /**
     * @param statisticalNumber 
     *      statistical number to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setStatisticalNumber(final String statisticalNumber) {
        this.statisticalNumber = statisticalNumber;
        return this;
    }

    /**
     * @return coutry ISO code
     */
    public final String getCountryIsoCode() {
        return countryIsoCode;
    }

    /**
     * @param countryIsoCode
     *      country ISO code to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setCountryIsoCode(final String countryIsoCode) {
        this.countryIsoCode = countryIsoCode;
        return this;
    }

    /**
     * Gets companyCategory.
     *
     * @return value of companyCategory
     */
    public final String getCompanyCategory() {
        return companyCategory;
    }

    /**
     * Sets companyCategory.
     *
     * @param companyCategory the companyCategory to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setCompanyCategory(final String companyCategory) {
        this.companyCategory = companyCategory;
        return this;
    }

    /**
     * Gets status.
     *
     * @return value of status
     */
    public final String getStatus() {
        return status;
    }

    /**
     * Sets status.
     *
     * @param status the status to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setStatus(final String status) {
        this.status = status;
        return this;
    }

    /**
     * Gets statusChange.
     *
     * @return value of statusChange
     */
    public final String getStatusChange() {
        return statusChange;
    }

    /**
     * Sets statusChange.
     *
     * @param statusChange the statusChange to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setStatusChange(final String statusChange) {
        this.statusChange = statusChange;
        return this;
    }

    /**
     * Gets nace.
     *
     * @return value of nace
     */
    public final String getNace() {
        return nace;
    }

    /**
     * Sets nace.
     *
     * @param nace the nace to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setNace(final String nace) {
        this.nace = nace;
        return this;
    }

    /**
     * Gets numberOfEmployees.
     *
     * @return value of numberOfEmployees
     */
    public final String getNumberOfEmployees() {
        return numberOfEmployees;
    }

    /**
     * Sets numberOfEmployees.
     *
     * @param numberOfEmployees the numberOfEmployees to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setNumberOfEmployees(final String numberOfEmployees) {
        this.numberOfEmployees = numberOfEmployees;
        return this;
    }

    /**
     * Gets foundationDate.
     *
     * @return value of foundationDate
     */
    public final String getFoundationDate() {
        return foundationDate;
    }

    /**
     * Sets foundationDate.
     *
     * @param foundationDate the foundationDate to set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setFoundationDate(final String foundationDate) {
        this.foundationDate = foundationDate;
        return this;
    }

    /**
     * @return group id. Etalon groupId has following format group_etalon_<id>. Note that this group id is only for
     * matching purposes, isn't saved.
     */
    
    @JsonIgnore
    @Override
    public final String getGroupId() {
        return "group_ETALON_body_" + sha256Hex(id);
    }

    /**
     * @return digest2
     */
    public final String getDigest2() {
        return digest2;
    }

    /**
     * @param digest2
     *      digest to be set
     * @return this instance for chaining
     */
    public final BVDEtalonBody setDigest2(final String digest2) {
        this.digest2 = digest2;
        return this;
    }    
}
