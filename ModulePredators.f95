!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Lotka Volterra
! DATE          : July 2013
! REVISION      : Ricardo Miranda
! DESCRIPTION   : Example program of Functional Core, Imperative Shell according to
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
 
Module ModulePredators

    implicit none

    !Types---------------------------------------------------------------------
    
    public :: T_Predators
    type      T_Predators
        private
        real(8), pointer :: PopulationSize
        !Population is computed every time step
        
        real(8), pointer :: IncreaseRate
        real(8), pointer :: DecreaseRate
    end type  T_Predators

    !--------------------------------------------------------------------------

    private 

    !Subroutines---------------------------------------------------------------

    !Constructor
    public  :: ConstructPredators
    private ::      AllocateInstance
    private ::      InitializeValues
    private :: AllocateReplica

    !Selector
    public  :: GetPredatorsPopulationSize
    public  :: GetPredatorsIncreaseRate
    public  :: GetPredatorsDecreaseRate
                         
    !Modifier
    public  :: ModifyPredatorsPopulation
    private ::      Death
    private ::      Increase

    !Destructor
    public  :: KillPredators                                                    
    private ::      DeAllocateInstance
    public  :: PredatorsGarbageCollector
    
    !Interfaces----------------------------------------------------------------

    !--------------------------------------------------------------------------
    
    contains


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    function ConstructPredators(IniPopulation, IncreaseRate, DecreaseRate) 

        !Arguments---------------------------------------------------------------
        real(8), intent(IN)                             :: IniPopulation
        real(8), intent(IN)                             :: IncreaseRate
        real(8), intent(IN)                             :: DecreaseRate
        
        !Return------------------------------------------------------------------
        type (T_Predators), pointer                          :: ConstructPredators

        !Local-------------------------------------------------------------------
        type (T_Predators), pointer                          :: NewObjPredators

        !------------------------------------------------------------------------

        call AllocateInstance(NewObjPredators)
        call InitializeValues(NewObjPredators, IniPopulation, IncreaseRate, DecreaseRate)

        ConstructPredators => NewObjPredators

        !----------------------------------------------------------------------

    end function ConstructPredators
 
    !--------------------------------------------------------------------------
    
    subroutine AllocateInstance(NewObjPredators)

        !Arguments-------------------------------------------------------------
        type (T_Predators), pointer                          :: NewObjPredators
                                                    
        !Local-----------------------------------------------------------------


        !Allocates new instance
        allocate (NewObjPredators)
        allocate (NewObjPredators%PopulationSize)
        allocate (NewObjPredators%IncreaseRate)
        allocate (NewObjPredators%DecreaseRate)

    end subroutine AllocateInstance
 
    !--------------------------------------------------------------------------
    
    subroutine InitializeValues(NewObjPredators, IniPopulation, IncreaseRate, DecreaseRate)

        !Arguments-------------------------------------------------------------
        type (T_Predators), pointer                     :: NewObjPredators
        real(8), intent(IN)                             :: IniPopulation
        real(8), intent(IN)                             :: IncreaseRate
        real(8), intent(IN)                             :: DecreaseRate
                                                    
        !Local-----------------------------------------------------------------

        NewObjPredators%PopulationSize = IniPopulation
        NewObjPredators%IncreaseRate   = IncreaseRate
        NewObjPredators%DecreaseRate   = DecreaseRate

    end subroutine InitializeValues

    !--------------------------------------------------------------------------
    
    function AllocateReplica(ObjPredators) 


        !Arguments-------------------------------------------------------------
        type (T_Predators), pointer                               :: ObjPredators
                                                    
        !Return----------------------------------------------------------------
        type (T_Predators), pointer                               :: AllocateReplica

        !Local-----------------------------------------------------------------
        type (T_Predators), pointer                               :: NewObjPredators


        !Allocates new values
        allocate (NewObjPredators)
        allocate (NewObjPredators%PopulationSize)
        
        NewObjPredators%PopulationSize = -99999.9
        !New value

        NewObjPredators%IncreaseRate   => ObjPredators%IncreaseRate
        NewObjPredators%DecreaseRate   => ObjPredators%DecreaseRate

        AllocateReplica => NewObjPredators

    end function AllocateReplica


    !--------------------------------------------------------------------------


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SE

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    

    !--------------------------------------------------------------------------
    
    function GetPredatorsPopulationSize (ObjPredators)

        !Arguments-------------------------------------------------------------
        type (T_Predators), pointer                          :: ObjPredators

        !Return----------------------------------------------------------------
        real(8)                                              :: GetPredatorsPopulationSize

        !----------------------------------------------------------------------

        GetPredatorsPopulationSize = ObjPredators%PopulationSize

    end function GetPredatorsPopulationSize
    
    !--------------------------------------------------------------------------
    
    function GetPredatorsIncreaseRate (ObjPredators)

        !Arguments-------------------------------------------------------------
        type (T_Predators), pointer                          :: ObjPredators

        !Return----------------------------------------------------------------
        real(8)                                              :: GetPredatorsIncreaseRate

        !----------------------------------------------------------------------

        GetPredatorsIncreaseRate = ObjPredators%IncreaseRate

    end function GetPredatorsIncreaseRate
    
    !--------------------------------------------------------------------------
    
    function GetPredatorsDecreaseRate (ObjPredators)

        !Arguments-------------------------------------------------------------
        type (T_Predators), pointer                          :: ObjPredators

        !Return------------------------------------------------------------------
        real(8)                                              :: GetPredatorsDecreaseRate

        !----------------------------------------------------------------------

        GetPredatorsDecreaseRate = ObjPredators%DecreaseRate

    end function GetPredatorsDecreaseRate
    
    !--------------------------------------------------------------------------
    
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODI

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    function ModifyPredatorsPopulation(ObjPredators, PreyPopulationSize, DT)

        !Arguments-------------------------------------------------------------
        type (T_Predators),  pointer                   :: ObjPredators
        real(8), intent(IN)                            :: PreyPopulationSize
        real(8), intent(IN)                            :: DT
                                                    
        !Return------------------------------------------------------------------
        type (T_Predators), pointer                    :: ModifyPredatorsPopulation

        !Local-------------------------------------------------------------------
        type (T_Predators), pointer                    :: NewObjPredators

        !----------------------------------------------------------------------

        NewObjPredators => AllocateReplica(ObjPredators)

        NewObjPredators%PopulationSize = ObjPredators%PopulationSize                                            &
                                       + Death   (ObjPredators%PopulationSize, ObjPredators%DecreaseRate, DT)   &
                                       + Increase(ObjPredators%PopulationSize, ObjPredators%IncreaseRate, PreyPopulationSize, DT)

        ModifyPredatorsPopulation => NewObjPredators

    end function ModifyPredatorsPopulation

    !--------------------------------------------------------------------------

    function Death(PredatorsPopulationSize, DecreaseRate, DT)
        !Arguments-------------------------------------------------------------
        real(8), pointer, intent(IN)                   :: PredatorsPopulationSize
        real(8), intent(IN)                            :: DecreaseRate
        real(8), intent(IN)                            :: DT
                                                    
        !Return----------------------------------------------------------------
        real(8)                                        :: Death

        !Local-----------------------------------------------------------------

        !----------------------------------------------------------------------

        Death =-1.0 * PredatorsPopulationSize * DecreaseRate * DT

        !----------------------------------------------------------------------

    end function Death

    !--------------------------------------------------------------------------

    function Increase(PredatorsPopulationSize, IncreaseRate, PreyPopulationSize, DT)
        !Arguments-------------------------------------------------------------
        real(8), pointer, intent(IN)                   :: PredatorsPopulationSize
        real(8), intent(IN)                            :: IncreaseRate
        real(8), intent(IN)                            :: PreyPopulationSize
        real(8), intent(IN)                            :: DT
                                                    
        !Return----------------------------------------------------------------
        real(8)                                        :: Increase

        !Local-----------------------------------------------------------------

        Increase = PredatorsPopulationSize * IncreaseRate * PreyPopulationSize * DT

        !----------------------------------------------------------------------

    end function Increase

    !--------------------------------------------------------------------------


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    subroutine KillPredators(ObjPredators)

        !Arguments---------------------------------------------------------------
        type (T_Predators),     pointer                      :: ObjPredators

        !Return------------------------------------------------------------------         

        !External----------------------------------------------------------------

        !Local-------------------------------------------------------------------

        !------------------------------------------------------------------------

        !Deallocates Instance
        call DeallocateInstance (ObjPredators)

        !------------------------------------------------------------------------

    end subroutine KillPredators
        

    !------------------------------------------------------------------------
    
    
    subroutine DeallocateInstance (ObjPredators)

        !Arguments-------------------------------------------------------------
        type (T_Predators), pointer                          :: ObjPredators
                                                    
        !Local-----------------------------------------------------------------

        !------------------------------------------------------------------------

        !Deallocates instance
        deallocate (ObjPredators%PopulationSize)
        deallocate (ObjPredators%IncreaseRate)
        deallocate (ObjPredators%DecreaseRate)
        deallocate (ObjPredators)
            
    end subroutine DeallocateInstance

    !--------------------------------------------------------------------------

    subroutine PredatorsGarbageCollector (ObjPredators)

        !Arguments-------------------------------------------------------------
        type (T_Predators), pointer                          :: ObjPredators
                                                    
        !Local-----------------------------------------------------------------

        !------------------------------------------------------------------------

        !Partially deallocates instance
        deallocate (ObjPredators%PopulationSize)
        deallocate (ObjPredators)
            
    end subroutine PredatorsGarbageCollector

    !--------------------------------------------------------------------------

end module ModulePredators








