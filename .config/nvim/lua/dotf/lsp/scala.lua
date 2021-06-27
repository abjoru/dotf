local M = {}

function M.setup(metals, capabilities, dap, diag)
  if not capabilities then configureCapabilities(capabilities) end

  metals.settings = {
    showImplicitArguments = true,
    showInferredType = true,
    fallbackScalaVersion = "2.13.6",
    excludedPackages = {
      "akka.actor.typed.javadsl",
      "com.github.swagger.akka.javadsl",
      "akka.stream.javadsl"
    }
  }

  metals.init_options.statusBarProvider = "on"
  metals.handlers["textDocument/publishDiagnostics"] = diag
  metals.capabilities = capabilities

  if not dap then 
    configureDap(dap)
    metals.on_attach = function(client, bufnr)
      require('metals').setup_dap()
    end
  end
end

local function configureCapabilities(capabilities)
end

local function configureDap(dap)
  -- For that they usually provide a `console` option in their |dap-configuration|.
  -- The supported values are usually called `internalConsole`, `integratedTerminal`
  -- and `externalTerminal`.
  dap.configurations.scala = {
    {
      type = "scala",
      request = "launch",
      name = "Run",
      metals = {
        runType = "run",
        args = { "firstArg", "secondArg", "thirdArg" },
      },
    },
    {
      type = "scala",
      request = "launch",
      name = "Test File",
      metals = {
        runType = "testFile",
      },
    },
    {
      type = "scala",
      request = "launch",
      name = "Test Target",
      metals = {
        runType = "testTarget",
      },
    },
  }
end

return M
