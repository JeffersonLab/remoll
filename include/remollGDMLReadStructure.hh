// -*- coding: utf-8 -*-
// vim: ai ts=2 sts=2 et sw=2 ft=cpp
// author : Prakash
// date   : 2024-02-18

#pragma once

#include <map>
#include <string>

#include "G4GDMLReadStructure.hh"

class remollGDMLReadStructure : public G4GDMLReadStructure {
  public:
    remollGDMLReadStructure();

    void RotationRead(const xercesc::DOMElement* const vectorElement, G4RotationMatrix& rot);
    G4LogicalVolume* FileRead(const xercesc::DOMElement* const fileElement);
    void PhysvolRead( const xercesc::DOMElement* const physvolElement, G4AssemblyVolume* pAssembly=0);
    void ParametersRead(const xercesc::DOMElement* const element);
    void Volume_contentRead( const xercesc::DOMElement* const volumeElement) override;

  private:
    std::map<std::string,std::vector<std::function<void(G4RotationMatrix&,G4ThreeVector)>>> rotations;
};
