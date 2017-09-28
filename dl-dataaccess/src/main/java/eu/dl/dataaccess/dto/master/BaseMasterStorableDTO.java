package eu.dl.dataaccess.dto.master;

import javax.persistence.MappedSuperclass;

import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.BaseParsedAndCleanStorableDTO;

/**
 * Common metadata for master storable objects.
 */
@MappedSuperclass
@Transformable
abstract class BaseMasterStorableDTO extends BaseParsedAndCleanStorableDTO {


}
