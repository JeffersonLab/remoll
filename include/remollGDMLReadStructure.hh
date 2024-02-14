#pragma once

#include "G4LogicalVolume.hh"
#include "G4VPhysicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4LogicalVolumeStore.hh"
#include "G4PhysicalVolumeStore.hh"
#include "G4AssemblyVolume.hh"
#include "G4ReflectionFactory.hh"
#include "G4PVDivisionFactory.hh"
#include "G4LogicalBorderSurface.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4GDMLReadStructure.hh"


class remollGDMLReadStructure : public G4GDMLReadStructure {
    public:
    remollGDMLReadStructure():G4GDMLReadStructure(){ }

    G4LogicalVolume* FileReads(const xercesc::DOMElement* const fileElement)
    {
        G4String name;
        G4String volname;

        const xercesc::DOMNamedNodeMap* const attributes =
            fileElement->getAttributes();
        XMLSize_t attributeCount = attributes->getLength();

        for(XMLSize_t attribute_index = 0; attribute_index < attributeCount;
                ++attribute_index)
        {
            xercesc::DOMNode* attribute_node = attributes->item(attribute_index);

            if(attribute_node->getNodeType() != xercesc::DOMNode::ATTRIBUTE_NODE)
            {
                continue;
            }

            const xercesc::DOMAttr* const attribute =
                dynamic_cast<xercesc::DOMAttr*>(attribute_node);
            if(attribute == nullptr)
            {
                G4Exception("G4GDMLReadStructure::FileRead()", "InvalidRead",
                        FatalException, "No attribute found!");
                return nullptr;
            }
            const G4String attName  = Transcode(attribute->getName());
            const G4String attValue = Transcode(attribute->getValue());

            if(attName == "name")
            {
                name = attValue;
            }
            else if(attName == "volname")
            {
                volname = attValue;
            }
        }

        const G4bool isModule = true;
        remollGDMLReadStructure structure;
        structure.Read(name, validate, isModule);

        // Register existing auxiliar information defined in child module
        //
        const G4GDMLAuxMapType* aux = structure.GetAuxMap();
        if(!aux->empty())
        {
            for(auto pos = aux->cbegin(); pos != aux->cend(); ++pos)
            {
                auxMap.insert(std::make_pair(pos->first, pos->second));
            }
        }

        // Return volume structure from child module
        //
        if(volname.empty())
        {
            return structure.GetVolume(structure.GetSetup("Default"));
            //return this->GetVolume(this->GetSetup("Default"));
        }
        else
        {
            return structure.GetVolume(structure.GenerateName(volname));
            //return this->GetVolume(this->GenerateName(volname));
        }
    }


    void PhysvolReads( const xercesc::DOMElement* const physvolElement, G4AssemblyVolume* pAssembly=0)
    {
        G4String name;
        G4LogicalVolume* logvol    = nullptr;
        G4AssemblyVolume* assembly = nullptr;
        G4ThreeVector position(0.0, 0.0, 0.0);
        G4ThreeVector rotation(0.0, 0.0, 0.0);
        G4ThreeVector rotationzyx(0.0, 0.0, 0.0);
        bool zyx_requested = false;
        G4ThreeVector scale(1.0, 1.0, 1.0);
        G4int copynumber = 0;

        const xercesc::DOMNamedNodeMap* const attributes =
            physvolElement->getAttributes();
        XMLSize_t attributeCount = attributes->getLength();

        for(XMLSize_t attribute_index = 0; attribute_index < attributeCount;
                ++attribute_index)
        {
            xercesc::DOMNode* attribute_node = attributes->item(attribute_index);

            if(attribute_node->getNodeType() != xercesc::DOMNode::ATTRIBUTE_NODE)
            {
                continue;
            }

            const xercesc::DOMAttr* const attribute =
                dynamic_cast<xercesc::DOMAttr*>(attribute_node);
            if(attribute == nullptr)
            {
                G4Exception("G4GDMLReadStructure::PhysvolRead()", "InvalidRead",
                        FatalException, "No attribute found!");
                return;
            }
            const G4String attName  = Transcode(attribute->getName());
            const G4String attValue = Transcode(attribute->getValue());

            if(attName == "name")
            {
                name = attValue;
            }
            if(attName == "copynumber")
            {
                copynumber = eval.EvaluateInteger(attValue);
            }
        }

        for(xercesc::DOMNode* iter = physvolElement->getFirstChild(); iter != nullptr;
                iter                   = iter->getNextSibling())
        {
            if(iter->getNodeType() != xercesc::DOMNode::ELEMENT_NODE)
            {
                continue;
            }

            const xercesc::DOMElement* const child =
                dynamic_cast<xercesc::DOMElement*>(iter);
            if(child == nullptr)
            {
                G4Exception("G4GDMLReadStructure::PhysvolRead()", "InvalidRead",
                        FatalException, "No child found!");
                return;
            }
            const G4String tag = Transcode(child->getTagName());

            if(tag == "volumeref")
            {
                const G4String& child_name = GenerateName(RefRead(child));
                assembly                   = GetAssembly(child_name);
                if(assembly == nullptr)
                {
                    logvol = GetVolume(child_name);
                }
            }
            else if(tag == "file")
            {
                logvol = FileReads(child);
            }
            else if(tag == "position")
            {
                VectorRead(child, position);
            }
            else if(tag == "rotation")
            {
                VectorRead(child, rotation);
            }
            else if(tag == "rotationzyx")
            {
                VectorRead(child, rotationzyx);
                zyx_requested = true;
            }
            else if(tag == "scale")
            {
                VectorRead(child, scale);
            }
            else if(tag == "positionref")
            {
                position = GetPosition(GenerateName(RefRead(child)));
            }
            else if(tag == "rotationref")
            {
                rotation = GetRotation(GenerateName(RefRead(child)));
            }
            else if(tag == "scaleref")
            {
                scale = GetScale(GenerateName(RefRead(child)));
            }
            else
            {
                G4String error_msg = "Unknown tag in physvol: " + tag;
                G4Exception("G4GDMLReadStructure::PhysvolRead()", "ReadError",
                        FatalException, error_msg);
                return;
            }
        }

        G4Transform3D transform;
        if(!zyx_requested){
            transform = G4Transform3D(GetRotationMatrix(rotation).inverse(), position);
        } else {
            G4RotationMatrix rot;

            rot.rotateY(rotationzyx.y());
            rot.rotateZ(rotationzyx.z());
            rot.rotateX(rotationzyx.x());
            rot.rectify();  // Rectify matrix from possible roundoff errors

            transform = G4Transform3D(rot.inverse(),position);
        }

        transform = transform * G4Scale3D(scale.x(), scale.y(), scale.z());

        if(pAssembly != nullptr)  // Fill assembly structure
        {
            if(assembly != nullptr)  // Case of recursive assemblies
            {
                pAssembly->AddPlacedAssembly(assembly, transform);
            }
            if(logvol == nullptr)
            {
                return;
            }
            pAssembly->AddPlacedVolume(logvol, transform);
        }
        else  // Generate physical volume tree or do assembly imprint
        {
            if(assembly != nullptr)
            {
                assembly->MakeImprint(pMotherLogical, transform, 0, check);
            }
            else
            {
                if(logvol == nullptr)
                {
                    return;
                }
                G4String pv_name           = logvol->GetName() + "_PV";
                G4PhysicalVolumesPair pair = G4ReflectionFactory::Instance()->Place(
                        transform, pv_name, logvol, pMotherLogical, false, copynumber, check);

                if(pair.first != nullptr)
                {
                    GeneratePhysvolName(name, pair.first);
                }
                if(pair.second != nullptr)
                {
                    GeneratePhysvolName(name, pair.second);
                }
            }
        }
    }

    void ParametersRead(const xercesc::DOMElement* const element) {

        G4ThreeVector rotation(0.0, 0.0, 0.0);
        G4ThreeVector rotationzyx(0.0, 0.0, 0.0);
        G4ThreeVector position(0.0, 0.0, 0.0);
        bool zyx_requested = false;

        G4GDMLParameterisation::PARAMETER parameter;
        parameter.pRot = new G4RotationMatrix();

        for(xercesc::DOMNode* iter = element->getFirstChild(); iter != nullptr;
                iter                   = iter->getNextSibling())
        {
            if(iter->getNodeType() != xercesc::DOMNode::ELEMENT_NODE)
            {
                continue;
            }

            const xercesc::DOMElement* const child =
                dynamic_cast<xercesc::DOMElement*>(iter);
            if(child == nullptr)
            {
                G4Exception("G4GDMLReadParamvol::ParametersRead()", "InvalidRead",
                        FatalException, "No child found!");
                return;
            }
            const G4String tag = Transcode(child->getTagName());
            if(tag == "rotation")
            {
                VectorRead(child, rotation);
            }
            else if(tag == "rotationzyx")
            {
                VectorRead(child, rotationzyx);
                zyx_requested = true;
            }
            else if(tag == "position")
            {
                VectorRead(child, position);
            }
            else if(tag == "positionref")
            {
                position = GetPosition(GenerateName(RefRead(child)));
            }
            else if(tag == "rotationref")
            {
                rotation = GetRotation(GenerateName(RefRead(child)));
            }
            else if(tag == "box_dimensions")
            {
                Box_dimensionsRead(child, parameter);
            }
            else if(tag == "trd_dimensions")
            {
                Trd_dimensionsRead(child, parameter);
            }
            else if(tag == "trap_dimensions")
            {
                Trap_dimensionsRead(child, parameter);
            }
            else if(tag == "tube_dimensions")
            {
                Tube_dimensionsRead(child, parameter);
            }
            else if(tag == "cone_dimensions")
            {
                Cone_dimensionsRead(child, parameter);
            }
            else if(tag == "sphere_dimensions")
            {
                Sphere_dimensionsRead(child, parameter);
            }
            else if(tag == "orb_dimensions")
            {
                Orb_dimensionsRead(child, parameter);
            }
            else if(tag == "torus_dimensions")
            {
                Torus_dimensionsRead(child, parameter);
            }
            else if(tag == "ellipsoid_dimensions")
            {
                Ellipsoid_dimensionsRead(child, parameter);
            }
            else if(tag == "para_dimensions")
            {
                Para_dimensionsRead(child, parameter);
            }
            else if(tag == "polycone_dimensions")
            {
                Polycone_dimensionsRead(child, parameter);
            }
            else if(tag == "polyhedra_dimensions")
            {
                Polyhedra_dimensionsRead(child, parameter);
            }
            else if(tag == "hype_dimensions")
            {
                Hype_dimensionsRead(child, parameter);
            }
            else
            {
                G4String error_msg = "Unknown tag in parameters: " + tag;
                G4Exception("G4GDMLReadParamvol::ParametersRead()", "ReadError",
                        FatalException, error_msg);
            }
        }



        if(!zyx_requested){
            parameter.pRot->rotateX(rotation.x());
            parameter.pRot->rotateY(rotation.y());
            parameter.pRot->rotateZ(rotation.z());
        } else {
            parameter.pRot->rotateY(rotationzyx.y());
            parameter.pRot->rotateZ(rotationzyx.z());
            parameter.pRot->rotateX(rotationzyx.x());
        }


        parameter.position = position;

        parameterisation->AddParameter(parameter);

    }

    void Volume_contentRead( const xercesc::DOMElement* const volumeElement) override
    {
        for(xercesc::DOMNode* iter = volumeElement->getFirstChild(); iter != nullptr;
                iter                   = iter->getNextSibling())
        {
            if(iter->getNodeType() != xercesc::DOMNode::ELEMENT_NODE)
            {
                continue;
            }

            const xercesc::DOMElement* const child =
                dynamic_cast<xercesc::DOMElement*>(iter);
            if(child == nullptr)
            {
                G4Exception("G4GDMLReadStructure::Volume_contentRead()", "InvalidRead",
                        FatalException, "No child found!");
                return;
            }
            const G4String tag = Transcode(child->getTagName());

            if((tag == "auxiliary") || (tag == "materialref") || (tag == "solidref"))
            {
                // These are already processed in VolumeRead()
            }
            else if(tag == "paramvol")
            {
                ParamvolRead(child, pMotherLogical);
            }
            else if(tag == "physvol")
            {
                PhysvolReads(child);
            }
            else if(tag == "replicavol")
            {
                G4int number = 1;
                const xercesc::DOMNamedNodeMap* const attributes = child->getAttributes();
                XMLSize_t attributeCount = attributes->getLength();
                for(XMLSize_t attribute_index = 0; attribute_index < attributeCount;
                        ++attribute_index)
                {
                    xercesc::DOMNode* attribute_node = attributes->item(attribute_index);
                    if(attribute_node->getNodeType() != xercesc::DOMNode::ATTRIBUTE_NODE)
                    {
                        continue;
                    }
                    const xercesc::DOMAttr* const attribute =
                        dynamic_cast<xercesc::DOMAttr*>(attribute_node);
                    if(attribute == nullptr)
                    {
                        G4Exception("G4GDMLReadStructure::Volume_contentRead()",
                                "InvalidRead", FatalException, "No attribute found!");
                        return;
                    }
                    const G4String attName  = Transcode(attribute->getName());
                    const G4String attValue = Transcode(attribute->getValue());
                    if(attName == "number")
                    {
                        number = eval.EvaluateInteger(attValue);
                    }
                }
                ReplicavolRead(child, number);
            }
            else if(tag == "divisionvol")
            {
                DivisionvolRead(child);
            }
            else if(tag == "loop")
            {
                LoopRead(child, &G4GDMLRead::Volume_contentRead);
            }
            else
            {
                G4cout << "Treating unknown GDML tag in volume '" << tag
                    << "' as GDML extension..." << G4endl;
            }
        }
    }

    void StructureRead( const xercesc::DOMElement* const structureElement)  override
    {
        G4cout << "G4GDML: Reading structure..." << G4endl;

        for(xercesc::DOMNode* iter = structureElement->getFirstChild();
                iter != nullptr; iter = iter->getNextSibling())
        {
            if(iter->getNodeType() != xercesc::DOMNode::ELEMENT_NODE)
            {
                continue;
            }

            const xercesc::DOMElement* const child =
                dynamic_cast<xercesc::DOMElement*>(iter);
            if(child == nullptr)
            {
                G4Exception("G4GDMLReadStructure::StructureRead()", "InvalidRead",
                        FatalException, "No child found!");
                return;
            }
            const G4String tag = Transcode(child->getTagName());

            if(tag == "bordersurface")
            {
                BorderSurfaceRead(child);
            }
            else if(tag == "skinsurface")
            {
                SkinSurfaceRead(child);
            }
            else if(tag == "volume")
            {
                VolumeRead(child);
            }
            else if(tag == "assembly")
            {
                AssemblyRead(child);
            }
            else if(tag == "loop")
            {
                LoopRead(child, &G4GDMLRead::StructureRead);
            }
            else
            {
                G4String error_msg = "Unknown tag in structure: " + tag;
                G4Exception("G4GDMLReadStructure::StructureRead()", "ReadError",
                        FatalException, error_msg);
            }
        }
    }


};
