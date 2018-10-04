package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.utils.ValidationUtils;

/**
 * CPV.
 */
@Transformable
public class CPV implements Validable {
    /**
     * CPV code of contract subject.
     */
    private String code;

    /**
     * CPV is for main subject.
     */
    private Boolean isMain;

    /**
     * @return the code
     */
    public final String getCode() {
        return code;
    }

    /**
     * @param code
     *            the code to set
     * @return this instance for chaining
     */
    public final CPV setCode(final String code) {
        this.code = code;
        return this;
    }

    /**
     * @return the isMain
     */
    public final Boolean getIsMain() {
        return isMain;
    }

    /**
     * @param isMain
     *            the isMain to set
     * @return this instance for chaining
     */
    public final CPV setIsMain(final Boolean isMain) {
        this.isMain = isMain;
        return this;
    }

    @Override
    @JsonIgnore
    public final CPV getValid() {
        return ValidationUtils.getValid(this, code);
    }
}
