module Qtility.Metrics.Class where

import RIO
import qualified System.Metrics as EKG
import System.Metrics.Prometheus.Concurrent.Registry (Registry)
import System.Remote.Monitoring.Prometheus (Server (..))

class HasMetricsServer env where
  metricsServerL :: Lens' env Server

instance HasMetricsServer Server where
  metricsServerL = id

class HasEKGStore env where
  ekgStoreL :: Lens' env EKG.Store

instance HasEKGStore EKG.Store where
  ekgStoreL = id

instance HasEKGStore Server where
  ekgStoreL = lens ekgStore (\server e -> server {ekgStore = e})

class HasPrometheusRegistry env where
  prometheusRegistryL :: Lens' env Registry

instance HasPrometheusRegistry Registry where
  prometheusRegistryL = id

instance HasPrometheusRegistry Server where
  prometheusRegistryL = lens prometheusRegistry (\server e -> server {prometheusRegistry = e})
