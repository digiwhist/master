package eu.dl.dataaccess.dto.parsed;

import java.util.ArrayList;
import java.util.List;

// TODO: Auto-generated Javadoc
/**
 * Public official.
 */
public class ParsedPublicOfficial extends BaseParsedStorableDTO implements Parsable {

    /**
     * Public officials givenName.
     */
    private String givenName;

    /**
     * Public officials familyName.
     */
    private String familyName;

    /**
     * Public officials fullName.
     */
    private String fullName;

    /**
     * Public officials gender.
     */
    private String gender;

    /**
     * Public officials year of birth.
     */
    private String yearOfBirth;

    /**
     * Public officials date of birth.
     */
    private String dateOfBirth;

    /**
     * Public officials year of death.
     */
    private String yearOfDeath;

    /**
     * Public officials date of death.
     */
    private String dateOfDeath;

    /**
     * Public officials collection of positions.
     */
    private List<ParsedPosition> positions;

    /**
     * Public officials collection of parties.
     */
    private List<ParsedParty> parties;

    /**
     * Public officials country.
     */
    private String country;

    private String region;


    /**
     * Gets the given name.
     *
     * @return the given name
     */
    public final String getGivenName() {
        return givenName;
    }


    /**
     * Sets the given name.
     *
     * @param givenName
     *            the given name
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setGivenName(final String givenName) {
        this.givenName = givenName;
        return this;
    }


    /**
     * Gets the family name.
     *
     * @return the family name
     */
    public final String getFamilyName() {
        return familyName;
    }


    /**
     * Sets the family name.
     *
     * @param familyName
     *            the family name
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setFamilyName(final String familyName) {
        this.familyName = familyName;
        return this;
    }


    /**
     * Gets the full name.
     *
     * @return the full name
     */
    public final String getFullName() {
        return fullName;
    }


    /**
     * Sets the full name.
     *
     * @param name
     *            the name
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setFullName(final String name) {
        this.fullName = name;
        return this;
    }


    /**
     * Gets the gender.
     *
     * @return the gender
     */
    public final String getGender() {
        return gender;
    }


    /**
     * Sets the gender.
     *
     * @param gender
     *            the gender
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setGender(final String gender) {
        this.gender = gender;
        return this;
    }


    /**
     * Gets the year of birth.
     *
     * @return the year of birth
     */
    public final String getYearOfBirth() {
        return yearOfBirth;
    }


    /**
     * Sets the year of birth.
     *
     * @param yearOfBirth
     *            the year of birth
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setYearOfBirth(final String yearOfBirth) {
        this.yearOfBirth = yearOfBirth;
        return this;
    }


    /**
     * Gets the year of death.
     *
     * @return the year of death
     */
    public final String getYearOfDeath() {
        return yearOfDeath;
    }


    /**
     * Sets the year of death.
     *
     * @param yearOfDeath
     *            the year of death
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setYearOfDeath(final String yearOfDeath) {
        this.yearOfDeath = yearOfDeath;
        return this;
    }


    /**
     * Gets the date of birth.
     *
     * @return the date of birth
     */
    public final String getDateOfBirth() {
        return dateOfBirth;
    }


    /**
     * Sets the date of birth.
     *
     * @param dateOfBirth
     *            the date of birth
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setDateOfBirth(final String dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
        return this;
    }


    /**
     * Gets the date of death.
     *
     * @return the date of death
     */
    public final String getDateOfDeath() {
        return dateOfDeath;
    }


    /**
     * Sets the date of death.
     *
     * @param dateOfDeath
     *            the date of death
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setDateOfDeath(final String dateOfDeath) {
        this.dateOfDeath = dateOfDeath;
        return this;
    }


    /**
     * Gets the positions.
     *
     * @return the positions
     */
    public final List<ParsedPosition> getPositions() {
        return positions;
    }


    /**
     * Sets the positions.
     *
     * @param positions
     *            the positions
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setPositions(final List<ParsedPosition> positions) {
        this.positions = positions;
        return this;
    }


    /**
     * Gets the parties.
     *
     * @return the parties
     */
    public final List<ParsedParty> getParties() {
        return parties;
    }


    /**
     * Sets the parties.
     *
     * @param parties
     *            the parties
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setParties(final List<ParsedParty> parties) {
        this.parties = parties;
        return this;
    }


    /**
     * Gets the country.
     *
     * @return the country
     */
    public final String getCountry() {
        return country;
    }


    /**
     * Sets the country.
     *
     * @param country
     *            the country
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setCountry(final String country) {
        this.country = country;
        return this;
    }


    /**
     * Gets the region.
     *
     * @return the region
     */
    public final String getRegion() {
        return region;
    }


    /**
     * Sets the region.
     *
     * @param region
     *            the region
     * @return the parsed public official
     */
    public final ParsedPublicOfficial setRegion(final String region) {
        this.region = region;
        return this;
    }


    /**
     * Adds the position.
     *
     * @param parsedPosition
     *            the parsed position
     * @return the parsed public official
     */
    public final ParsedPublicOfficial addPosition(final ParsedPosition parsedPosition) {
        if (getPositions() == null) {
            setPositions(new ArrayList<>());
        }
        this.positions.add(parsedPosition);
        return this;
    }


    /**
     * Adds the party.
     *
     * @param parsedParty
     *            the parsed party
     * @return the parsed public official
     */
    public final ParsedPublicOfficial addParty(final ParsedParty parsedParty) {
        if (getParties() == null) {
            setParties(new ArrayList<>());
        }
        this.parties.add(parsedParty);
        return this;
    }
}
