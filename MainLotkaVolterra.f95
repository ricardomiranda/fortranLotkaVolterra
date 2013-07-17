!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Lotka Volterra
! DATE          : July 2013
! REVISION      : Ricardo Miranda
! DESCRIPTION   : Example program of Functional Core, Imperative LotkaVolterra according to
!                 Gary Bernhardt
!
!------------------------------------------------------------------------------
!
!This program is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public License 
!version 2, as published by the Free Software Foundation.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program; if not, write to the Free Software
!Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!
!------------------------------------------------------------------------------
 
program MohidLotkaVolterra

    use ModulePrey
    use ModulePredators

    implicit none

    type T_LV
        type(T_Prey), pointer       :: ObjPrey
        type(T_Predators), pointer  :: ObjPredators

        real(8)                     :: DT, NbrSteps
    end type T_LV

    type(T_LV), pointer :: ObjLotkaVolterra

    call ConstructMohidLotkaVolterra
    call ModifyLotkaVolterra
    call KillLotkaVolterra

    contains
    
    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine ConstructMohidLotkaVolterra

        call AllocateInstance
        call AskValues

    end subroutine ConstructMohidLotkaVolterra
    
    !--------------------------------------------------------------------------

    subroutine AskValues 

        !Local-------------------------------------------------------------------
        real(8) :: IniPreyPopulation
        real(8) :: IniPredatorsPopulation
        
        real(8) :: PreyBirthRate
        real(8) :: PreyDestroyRate
        real(8) :: PredatorsIncreaseRate
        real(8) :: PredatorsDeathRate

        !------------------------------------------------------------------------

        print*, "What is the time step?"
        read*,  ObjLotkaVolterra%DT

        print*, "How many time steps to compute?"
        read*,  ObjLotkaVolterra%NbrSteps

        print*, "What is the incial prey population?"
        read*,  IniPreyPopulation

        print*, "What is the incial predators population?"
        read*,  IniPredatorsPopulation

        print*, "What is the prey birth rate?"
        read*,  PreyBirthRate

        print*, "What is the prey destroy rate?"
        read*,  PreyDestroyRate

        print*, "What is the predators increase rate?"
        read*,  PredatorsIncreaseRate

        print*, "What is the predators death rate?"
        read*,  PredatorsDeathRate

        ObjLotkaVolterra%ObjPrey      => ConstructPrey      (IniPreyPopulation,      PreyBirthRate,         PreyDestroyRate)
        ObjLotkaVolterra%ObjPredators => ConstructPredators (IniPredatorsPopulation, PredatorsIncreaseRate, PredatorsDeathRate)

    end subroutine AskValues
    
    !--------------------------------------------------------------------------

    subroutine AllocateInstance
        
        allocate (ObjLotkaVolterra)

    end subroutine AllocateInstance
    
    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SE

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine ModifyLotkaVolterra 

        call Loop (ObjLotkaVolterra%NbrSteps)
    
    end subroutine ModifyLotkaVolterra
    
    !--------------------------------------------------------------------------

    recursive subroutine Loop (NbrSteps)
        
        !Return----------------------------------------------------------------         
        real(8), intent(IN) :: NbrSteps

        !Local-----------------------------------------------------------------
        type(T_Prey), pointer       :: NewObjPrey
        type(T_Predators), pointer  :: NewObjPredators

        real(8)                     :: PreyPopulationSize
        real(8)                     :: PredatorsPopulationSize

        !----------------------------------------------------------------------

cd1 :   if (NbrSteps .LE. 0.0) then
            print*, "Simulation terminated successfully."

        else   cd1
            PreyPopulationSize      = GetPreyPopulationSize      (ObjLotkaVolterra%ObjPrey)
            PredatorsPopulationSize = GetPredatorsPopulationSize (ObjLotkaVolterra%ObjPredators)

            print*, NbrSteps, PreyPopulationSize, PredatorsPopulationSize
        
            NewObjPrey     => ModifyPreyPopulation     (ObjLotkaVolterra%ObjPrey,      PredatorsPopulationSize, ObjLotkaVolterra%DT)
            NewObjPredators=> ModifyPredatorsPopulation(ObjLotkaVolterra%ObjPredators, PreyPopulationSize,      ObjLotkaVolterra%DT)

            call PreyGarbageCollector      (ObjLotkaVolterra%ObjPrey)
            call PredatorsGarbageCollector (ObjLotkaVolterra%ObjPredators)

            ObjLotkaVolterra%ObjPrey      => NewObjPrey
            ObjLotkaVolterra%ObjPredators => NewObjPredators
        
            call Loop (NbrSteps - ObjLotkaVolterra%DT)
        end if cd1
    
    end subroutine Loop
    
    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    subroutine KillLotkaVolterra

        call KillPrey     (ObjLotkaVolterra%ObjPrey)
        call KillPredators(ObjLotkaVolterra%ObjPredators)

        deallocate (ObjLotkaVolterra)

    end subroutine KillLotkaVolterra
    
    !--------------------------------------------------------------------------

end program MohidLotkaVolterra
