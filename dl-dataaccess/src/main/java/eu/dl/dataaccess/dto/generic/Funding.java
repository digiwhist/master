package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.utils.ClassUtils;
import eu.dl.dataaccess.utils.ValidationUtils;

/**
 * Funding. Funding sources used (if none listed, implicitly assuming own budget
 * of the body..).
 */
@Transformable
public class Funding implements Validable {
    /**
     * Name of the funding source.
     */
    private String source;

    /**
     * Source is from EU funds.
     */
    private Boolean isEuFund;

    /**
     * Refined classification of sources (such as type of Operational programme
     * etc..).
     */
    private String programme;

    /**
     * Ammount of money contributed from the source.
     */
    private Price amount;

    /**
     * Percentage of money contributed from the source.
     */
    private Integer proportion;

    /**
     * @return the source
     */
    public final String getSource() {
        return source;
    }

    /**
     * @param source
     *            the source to set
     * @return this instance for chaining
     */
    public final Funding setSource(final String source) {
        this.source = source;
        return this;
    }

    /**
     * @return the isEuFund
     */
    public final Boolean getIsEuFund() {
        return isEuFund;
    }

    /**
     * @param isEuFund
     *            the isEuFund to set
     * @return this instance for chaining
     */
    public final Funding setIsEuFund(final Boolean isEuFund) {
        this.isEuFund = isEuFund;
        return this;
    }

    /**
     * @return the programme
     */
    public final String getProgramme() {
        return programme;
    }

    /**
     * @param programme
     *            the programme to set
     * @return this instance for chaining
     */
    public final Funding setProgramme(final String programme) {
        this.programme = programme;
        return this;
    }

    /**
     * @return the amount
     */
    public final Price getAmount() {
        return amount;
    }

    /**
     * @param amount
     *            the amount to set
     * @return this instance for chaining
     */
    public final Funding setAmount(final Price amount) {
        this.amount = amount;
        return this;
    }

    /**
     * @return the proportion
     */
    public final Integer getProportion() {
        return proportion;
    }

    /**
     * @param proportion
     *            the proportion to set
     * @return this instance for chaining
     */
    public final Funding setProportion(final Integer proportion) {
        this.proportion = proportion;
        return this;
    }

    @Override
    @JsonIgnore
    public final Funding getValid() {
        setAmount(ClassUtils.removeNonsenses(amount));
        
        return ValidationUtils.getValid(this, source, isEuFund);
    }
}
