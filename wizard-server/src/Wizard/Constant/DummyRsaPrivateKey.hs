module Wizard.Constant.DummyRsaPrivateKey where

import qualified Crypto.PubKey.RSA as RSA
import Data.Maybe (fromJust)

import Shared.Common.Util.Crypto

dummyRsaPrivateKey :: RSA.PrivateKey
dummyRsaPrivateKey =
  fromJust . readRSAPrivateKey $
    "-----BEGIN RSA PRIVATE KEY-----\n\
    \MIIJKgIBAAKCAgEA7NwA82Lb+bf5aHkiEt4PUSyk0mHUKdwzFnvnpt7XbcYXcu4p\n\
    \WW7sd885nHRwcdq07fgw5/e+HC/sXbso2in7wyawmmGMAeGs6kBq+N2IK2p+Mp4L\n\
    \+Umwn1rS8+aMr9fnrQZkwWCweQFjpG1F29Zh65AG0Ben/O25l1QFn7+XRTVKMl1+\n\
    \GqVBllVYzVMTJpSnxNIpVTOOa07/d9xRxX2fEy3yeaYJXsX5D2mrwXe5DyD6joix\n\
    \6RKwJMjb/suvv5+qGWAV0ieEhDHofO8U5I2aKQrx5g7AIRG/Yc/hBomlgyjEP6Pr\n\
    \On8KEctt9SsMUDbcPwjKbZD8uAKgC66ME6LmIldHsixaREOM3r0lG/T0THlEtyuD\n\
    \64Nnv4q/lBNbETZza05eHN2X++mO1GRXQPz/x+DhrdyMVoAFL9wfMgSkt4dMjgO8\n\
    \+6STCdc+DEatmrx25T4cLa3+P5wYpaZvNXqmOZ9G4EVlfBANmXydRu5oLe5zt3h8\n\
    \PMlhaGhByHvemVhTFNVri+/p46E6lCU1iV+gWA8oI6iUjSL+yD0XdWAEWZ9CW5kC\n\
    \HlwN9Sv0/pPKLj6vyUuVlgXPAtgdhqoPqFUrCbgtarsSsnWPNgROeKz/IrxgosPZ\n\
    \EaZ54DHNY+CfEy3MYlwM4BwkSEdSotoypCf0bt4CrDaa2sWomWDz8l9aClcCAwEA\n\
    \AQKCAgAtMOydg0clJgzX+gSeRO/3NOVdnny0X+b/azSPigTgVOM0cmxBPACx+z+q\n\
    \siyiDHBo1um92G7VQe9/Eq6AsXHvkfov+lyUH1K2fwPp55qLbg4V6kGulni0bsfA\n\
    \8UP2aOMzB/1BSAljjPhN7utdPlIAd8JVAa5ze4flb4qDKDLcaYZ2hZAB1XXmEPKY\n\
    \ejk1Kze3PcHVF7kckD39yHNxkr4/Eo0o2J0BEBSs9CRpDUaptEkZ2MrZBU/GNOyE\n\
    \eKhXj0cNAGApcJDACCWrcJIz6TB4lDYDBt1xcclqCBeO/8QseiK3YbcHuiVZr86S\n\
    \8FuxEkaDYFH/BaKyGQ4gLHVdab8ugC/4zSGZW15HsnbdeSCFtI4HHEMiTvkXOS7Y\n\
    \/uPNja9MHoQLic3TqUzjiIBO4E20qGUjZVclaqB4UXSMqeLQQT8FY/4veez6Z+nb\n\
    \fv8B2dh3bm0VRci3X+LQU6GVoKJ/e7OaJh+7Kw+/6B4RuKv2IzO9BQH5z9s0kr+F\n\
    \50guTvVvbCNxUm4rPxYJ0rooLKMtDXHXLSxiVfXoWtAuJ93sjsTNGVYXOiqrc/My\n\
    \+hzd+5S+Y0FOfSDLyBVfr4cjIRLya7IGAvhesgVg4gdexYIqNcFWMSdS1FXs5LIV\n\
    \JHf8kKaAwIBQTY3Q+Sq83TZihXU29MyoE1HRkB04t6+zppq7AQKCAQEA+So7l68W\n\
    \Uo6V/gdZadTZxnVZdI240lHS5C5dUywPZ3sUu8qvxOJM39aIeih2WSpEz8Zl6ysw\n\
    \1EqLFeN/lFVXNkEgnLCcOtqpbecJIhhs8CiOlqg7e84/yaI7wry5V2mPBpFBu1bi\n\
    \Me9U5wyPDjWSJ3Nkm98+X7H3pmlBqx9ykRTtYwqQWTtWzqe3kE+xxI5M2KIjIQY7\n\
    \+RIiH5+amjhozaDWOoAM+I5g604LS3G5P7TMJ3f1W1uGDh4neFUtHNO4AYpEIEuW\n\
    \WQSoDc66f88lcqOGjW7PH9IljmMgKWBWL9HAxM/y+6oHSg/w0WtEeXVGXjxbWMUl\n\
    \y+u2E4qrHvqVhwKCAQEA81tay9Dhra8crCV1kD3FzHLARJeO3Gt1XeKSWrNrlbV5\n\
    \1OgP8gcjWFOhyvwHuGChPhyDgsVUJnMCGAb+9aTUW96T1LZ6G6qHns+nZ4VGL1md\n\
    \Gv6ZM2AMGAcIoabUiZoF3PIZ1gQXmW52isv4wu1Zyb9Lo8yuHpFga5wTXqA+tXoi\n\
    \LGIEegeIyLhZsx32Qjqg5vIB3ClFKMXdn9UHpkQR+L+UJe+hWkmT9AbzqkswMjCr\n\
    \vRl/SQMcTNcVt5EnB2hyAeN7/94rdWT82/XRPWIdOm8RBiYK7VWldMGwgBuAtIoU\n\
    \lw1pUk63bbTlTQL/dar2gK71T00CFHXJfLy93kwYsQKCAQEA9GW7fEYuFB5t+3UD\n\
    \/C3eyq4yZ4PXpNzDijP8MGfIWE0/AhCGpgz/MJPC3Gex5jXytwa405pS0/imQnsa\n\
    \mcm3uKzbKzaEl6oBhJ6Ys8vXlx2A9z/e0Mp2gPXTvmjVN2t/8gYqvnIzfp1t3w/P\n\
    \hjV/vZiPN+Ea0kzDSIR3+DzpaGtpibtD6XKt7BhQkonJKa4WrGslH/B4hQ+HTxOD\n\
    \VwA7tG6tk0Yk8uzdHhE4PPE4rsdNdO1SSiEU9eYX+w05Kcx18XTI7BCYUYqiuKBt\n\
    \CI6MAc1qXnIxUdoUIn0Sf+NRxqfseGAsyzN04O2P3ipkSOqCck6rnwAJ2eLDfpmG\n\
    \zeUnIwKCAQEAhfEHJVz/iZ/zERDBBzXos2GJT3d3avw+KgF1ejR8h3bN6pAbjqO9\n\
    \IXVOfLGA8ktFFDGPdJ91dIAia+UV2U/yjeoQxaf1x1pxsMgsJH8nJRKWtO/ilHNN\n\
    \1L+yUX7IRkAe7//UloXEE2/IERa5AViv7am7URlr0GlFf5T02EUsnqftPS5ptlyF\n\
    \PayxujNIgiVQwCv9OBcC+LEHDVysDCg1GAsh2EtvLImIxGw7GsStb0J4k+j/tMjg\n\
    \f652RoW+zIYBEecKLcczdjayMcK3eqiQ28ObxB2WMBDOs1eT9L6udS/2qzj3ehTX\n\
    \Qquo9V0sC7CgYi1p2U7SUT6POneLrVmG0QKCAQEAlAI0t382zT5EaUsDZc37tDnM\n\
    \V7L2R3hQF3PJGZO9HV0wvMBhzCIWWuqMKB2KxfmIAMN9dJnjocCrBvN/bIcW1JjG\n\
    \wkWR7bTz+m1r24hFEQPcpje0bqpUcyw3Mf8QwWPbgfH995+NR+odvktcZmSbnATF\n\
    \WTSFwCqwWeSzKJTmJBIzDTSiVDJBA50OvnAU/n1bU7m4teGde6d+FcgER8Eln68o\n\
    \infg6i9E3SGnBVCjxrHua4pDoiU4QLyF1Q5IpE8bD2RRtxOaC0ZsEV5Cj0rXuLuo\n\
    \DENsm0pq+dKiWQmOzYFJAHC6YDDwoG+Ud6/nGhQD0VQsxkFcPWihapl1+fSQxA==\n\
    \-----END RSA PRIVATE KEY-----"
