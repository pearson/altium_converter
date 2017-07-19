{*
 * Altium to KiCad converter script - configuration
 *
 * Copyright (C) 2017 CERN
 * @author Maciej Suminski <maciej.suminski@cern.ch>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you may find one here:
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * or you may search the http://www.gnu.org website for the version 2 license,
 * or you may write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 *}

// SCHEMATIC SYMBOL CONVERTER
/////////////////////////////

const
  // default parameter text height
  SCH_PARAM_TEXT_SIZE = 60;


// FOOTPRINT CONVERTER
//////////////////////

const
  // should the silkscreen texts be stroked (slows down the conversion,
  // makes texts ineditable, but they are exactly converted)
  //STROKE_SILK_TEXT = true;

  // 3D model path, if there is a file with the name matching the footprint,
  // the model will be assigned to the converted footprint
  MODEL_PATH = 'G:\Applications\Altium\CernLib\StepLib';

  // whether to use full path to the model or just the model name
  // (requires setting a variable in pcbnew, but then it is portable)
  USE_FULL_MODEL_PATH = false;


// Compatibility settings
  // approximate octagonal pads with rounded rectangles
  OCTAGON_TO_ROUNDRECT = false;

  // abort when one of unmapped layers is used
  ABORT_ON_UNKNOWN_LAYER = true;

  // KiCad pad names are limited to 4 characters, should truncated pad names
  // be allowed?
  TRUNCATE_PAD_NAMES = false;

  // Should truetype fonts be converted? WARNING: Using stroked font instead of
  // truetype will five *very* different results. You need to verify if
  // there are no collisions with other objects.
  CONVERT_TTF = false;


// defines layer mapping used for footprint conversion
function layerMapping(aLayer : TLayer) : TPCB_String;
begin
    // KiCad mechanical layers names:
    // Edge.Cuts, Margin, {F,B}.CrtYd, {F,B}.Adhes, {F,B}.Fab,
    // Eco{1,2}.User, Dwgs.User, Cmts.User

    case aLayer of
        //eDrillGuide:         result := '';
        //eKeepOutLayer:       result := '';
        eMechanical1:        result := 'Cmts.User';
        //eMechanical2:        result := '';
        eMechanical3:        result := 'Eco1.User';
        eMechanical4:        result := 'Eco2.User';
        //eMechanical5:        result := '';
        //eMechanical6:        result := '';
        //eMechanical7:        result := '';
        eMechanical8:        result := 'F.Fab';
        //eMechanical9:        result := '';
        //eMechanical10:       result := '';
        //eMechanical11:       result := '';
        //eMechanical12:       result := '';
        //eMechanical13:       result := '';
        eMechanical14:       result := 'Dwgs.User';
        eMechanical15:       result := 'F.CrtYd';
        //eMechanical16:       result := '';
        // ...mechanical layer numbering continues up to 32
        eDrillDrawing:       result := 'B.Fab';

        // not used in footprints?
        //eNoLayer:            result := '';
        //eInternalPlane1:     result := '';
        //eInternalPlane2:     result := '';
        //eInternalPlane3:     result := '';
        //eInternalPlane4:     result := '';
        //eInternalPlane5:     result := '';
        //eInternalPlane6:     result := '';
        //eInternalPlane7:     result := '';
        //eInternalPlane8:     result := '';
        //eInternalPlane9:     result := '';
        //eInternalPlane10:    result := '';
        //eInternalPlane11:    result := '';
        //eInternalPlane12:    result := '';
        //eInternalPlane13:    result := '';
        //eInternalPlane14:    result := '';
        //eInternalPlane15:    result := '';
        //eInternalPlane16:    result := '';
        //eMultiLayer:         result := '';
        //eConnectLayer:       result := '';
        //eBackGroundLayer:    result := '';
        //eDRCErrorLayer:      result := '';
        //eHighlightLayer:     result := '';
        //eGridColor1:         result := '';
        //eGridColor10:        result := '';
        //ePadHoleLayer:       result := '';
        //eViaHoleLayer:       result := '';
    end;
end;
